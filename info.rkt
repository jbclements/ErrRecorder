#lang setup/infotab

(define name "ErrRecorder")

(define blurb '((p "ErrRecorder extends DrRacket so that the "
                   "error display handler forwards the first "
                   "200 chars of any error message to a "
                   "global database that allows users to "
                   "suggest solutions.")))


(define drracket-tools '(("errrecorder-tool.ss")))
(define drracket-tool-names '("Error Recorder"))

(define categories '(devtools))
(define version "2011-02-04-21:08")
(define release-notes '((p "Initial Release")))


(define compile-omit-paths '())

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

(define homepage "http://www.errrecorder.org/")
#;(define url "http://schematics.sourceforge.net/")

