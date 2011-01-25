#lang racket

(require drracket/tool
         net/sendurl
         net/url
         net/uri-codec
         racket/date
         racket/gui
         mrlib/include-bitmap)

(provide tool@)


(define db-host "li21-127.members.linode.com")
(define db-submit-port 8022)
(define db-query-port 8021)

(define servlet-path (list (path/param "errrecorder" '())))

(define submit-url 
 (url "http" #f db-host db-submit-port #t servlet-path `() #f))

(define (query-error-url type-str msg-str)
  (url->string
   (url "http" #f db-host db-query-port #t servlet-path
        `((type . ,type-str)
          (msg . ,msg-str))
        #f)))


(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    ; Taken from drracket/debug.rkt to create clickable image.
    ; start
    
    (define arrow-cursor (make-object cursor% 'arrow))
    
    (define (clickable-snip-mixin snip%)
      (class snip%
        (init-rest args)
        (inherit get-flags set-flags get-admin get-extent)
        
        (define callback void)
        (define/public (set-callback cb) (set! callback cb))
        (define/public (get-callback) callback)
        
        (define in-bounds? #f)
        (define grabbed? #f)
        
        (define (set-clicked new-grabbed? new-in-bounds? dc)
          (let ([needs-invalidate? (not (eq? (and grabbed? in-bounds?)
                                             (and new-grabbed? new-in-bounds?)))])
            (set! grabbed? new-grabbed?)
            (set! in-bounds? new-in-bounds?)
            (when needs-invalidate?
              (invalidate dc))))
        
        (define/override (draw dc x y left top right bottom dx dy draw-caret)
          (super draw dc x y left top right bottom dx dy draw-caret)
          (when (and in-bounds? grabbed?)
            (let ([brush (send dc get-brush)]
                  [pen (send dc get-pen)])
              (let-values ([(w h) (get-w/h dc)])
                (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
                (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
                (send dc draw-rectangle x y w h)
                (send dc set-pen pen)
                (send dc set-brush brush)))))
        
        (define/override (on-event dc x y editorx editory evt)
          (let-values ([(w h) (get-w/h dc)])
            (let ([in-bounds? (and (<= (- (send evt get-x) x) w)
                                   (<= (- (send evt get-y) y) h))])
              (cond
                [(send evt button-down? 'left)
                 (set-clicked #t in-bounds? dc)]
                [(send evt button-up? 'left)
                 (let ([admin (send this get-admin)])
                   (when admin
                     (send (send admin get-editor) set-caret-owner #f 'global)))
                 (when (and grabbed? in-bounds?)
                   (callback))
                 (set-clicked #f in-bounds? dc)]
                [else
                 (set-clicked grabbed? in-bounds? dc)]))))
        
        (define/private (invalidate dc)
          (let ([admin (get-admin)])
            (when admin
              (let-values ([(w h) (get-w/h dc)])
                (send admin needs-update this 0 0 w h)))))
        
        (define/private (get-w/h dc)
          (let ([wb (box 0)]
                [hb (box 0)])
            (get-extent dc 0 0 wb hb #f #f #f #f)
            (values (unbox wb)
                    (unbox hb))))
        
        (define/override (adjust-cursor dc x y editorx editory event)
          arrow-cursor)
        
        (apply super-make-object args)
        (set-flags (cons 'handles-events (get-flags)))))
    
    (define (make-note% filename bitmap)
      (and (send bitmap ok?)
           (letrec ([note%
                     (class clickable-image-snip%
                       (inherit get-callback)
                       (define/public (get-image-name) filename)
                       (define/override (copy) 
                         (let ([n (new note%)])
                           (send n set-callback (get-callback))
                           n))
                       (super-make-object bitmap))])
             note%)))
    
    (define clickable-image-snip% (clickable-snip-mixin image-snip%))
    
    ; end
    
    ; errrecorder-note% : my clickalble image
    (define errrecorder-note% 
      (make-note% "syncheck.png"        
                  (include-bitmap (lib "icons/syncheck.png") 'png/mask)))
    
    ; string->post-bytes : string? -> bytes?
    ; converts string into url-encoded bytes
    (define (string->post-bytes s)
      (string->bytes/utf-8
       (form-urlencoded-encode s)))
    
    ; symbol->post-bytes : symbol? -> bytes?
    ; converts symbol into url-encoded bytes
    (define symbol->post-bytes
      (compose string->post-bytes symbol->string))
    
    ; bindings->post-bytes : (list (symbol? string?)) -> bytes?
    ; converts a list of symbol to string bindings into url-encoded bytes
    (define bindings->post-bytes
      (match-lambda
        [(list) #""]
        [(list (list (? symbol? sym) (? string? val)))
         (bytes-append (symbol->post-bytes sym) #"=" (string->post-bytes val))]
        [(list-rest (list (? symbol? sym) (? string? val)) bs)
         (bytes-append (symbol->post-bytes sym) #"=" (string->post-bytes val)
                       #"&" (bindings->post-bytes bs))]))
    
    ; extract-exn-type : exn? -> string?
    ; extracts the exn type out of the exn message
    (define (extract-exn-type exn)
      (let* ([exn-str (format "~v" exn)]
             [exn-lst (string->list exn-str)])
        (list->string (trim-exn (string->list (substring exn-str (find-start-exn exn-lst 0)))))))
    
    ; find-start-exn : list? int? -> list?
    ; finds the start of the exn message so you only get the exn type
    (define (find-start-exn loc cnt)
      (cond [(and (char=? (first loc) #\e) (char=? (second loc) #\x) (char=? (third loc) #\n)) cnt]
            [else (find-start-exn (rest loc) (+ cnt 1))]))
    
    ; trim-exn : list? -> list?
    ; trims the end of the exn message so you only get the exn type
    (define (trim-exn loc)
      (cond [(char=? (first loc) #\space) empty]
            [else (cons (first loc) (trim-exn (rest loc)))]))
    
    ; log-error-wrapper : (-> 'b) -> 'b
    ; evaluate the given function, spool the error message
    ; to the error log on a network failure
    (define (log-error-wrapper func)
      (with-handlers 
          ([exn:fail:network? 
            (λ (exn)
              (log-error 
               (format 
                "unable to connect to ErrRecorder server: ~a" 
                (exn-message exn))))])
        (func)))
    
    ; display-errrecorder-button : exn? string? -> nothing
    ; adds button to gui
    (define (display-errrecorder-button exn msg)
      (when errrecorder-note%
        (when (port-writes-special? (current-error-port))
          (let ([note (new errrecorder-note%)]
                [exn-type (extract-exn-type exn)])
            (send note set-callback 
                  (λ () (send-url 
                         (query-error-url (extract-exn-type exn)
                                          msg))))
            (write-special note (current-error-port))
            (display #\space (current-error-port))))))
    
    ; send-error-request : exn? string? -> nothing
    ; sends error information to server
    (define (send-error-request exn msg)
      (log-error-wrapper
       (λ () 
         (define in-port
           (post-pure-port
            submit-url 
            (bindings->post-bytes 
             `((type ,(extract-exn-type exn)) 
               (time ,(number->string (current-seconds)))
               (msg ,msg)))))
          ;; ignore the result:
          (close-input-port in-port))))
    
    ; errrecorder-language<%> : an empty interface
    (define errrecorder-language<%>
      (interface ()))
    
    ; errrecorder-language-extension : object? -> object?
    ; produces a language-extension/mixin that modifies the error-display-handler
    (define (errrecorder-language-extension super%)
      (class* super% (errrecorder-language<%>)
        
        (define/override (on-execute settings run-in-user-thread)
          (super on-execute settings run-in-user-thread)
          (run-in-user-thread
           (λ () (let ([current-error-display-handler (error-display-handler)])
                   (error-display-handler 
                    (make-errrecorder-error-display-handler current-error-display-handler))))))
        
        (super-new)))
    
    ; make-errrecorder-error-display-handler : string? exn? -> nothing
    ; adds errrecorder button to gui before normal display handler is called
    (define ((make-errrecorder-error-display-handler current-error-display-handler) msg exn)
      (send-error-request exn msg)
      (display-errrecorder-button exn msg)
      (current-error-display-handler msg exn))
    
    ; phase1 : nothing -> nothing
    ; extends language interfaces with errrecorder interface
    (define (phase1) (drracket:language:extend-language-interface 
                      errrecorder-language<%>
                      (λ (super%) (errrecorder-language-extension super%))))
    
    ; phase2 : nothing -> nothing
    ; does nothing but is required
    (define (phase2) (void))
    
    ))
