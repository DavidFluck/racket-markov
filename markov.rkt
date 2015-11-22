#lang racket
; Return a function that reads one line at a time from a file.
(define (make-line-reader filename)
  (let ([file (open-input-file filename)])
    (lambda ()
      (when (not (port-closed? file))
        (let ([next-line (read-line file)])
          (if (eof-object? next-line)
              (close-input-port file)
            next-line))))))