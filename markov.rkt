#lang racket
(require racket/generator)

; Create a closure that reads one line at a time from a file.
(define (make-line-reader filename)
  (let ([file (open-input-file filename)])
    (lambda ()
      (when (not (port-closed? file))
        (let ([next-line (read-line file)])
          (if (eof-object? next-line)
              (close-input-port file)
            next-line))))))

; Create a closure that returns a certain number of "words" from a file.
(define (make-line-collector filename num-elems)
  (let ([line-reader (make-line-reader filename)]
        [word-list '()])
    (lambda ()
      (cond
        [(< (length word-list) num-elems)
         (let ([next-line (line-reader)])
            (if (not (void? next-line))
                (set! word-list (append word-list (string-split next-line)))
              (void)))]
        [else
         (let ([ret-list (take word-list num-elems)])
           (begin
             (set! word-list (drop word-list num-elems))
             ret-list))]))))

(define (get-words num-elems buffer line-reader)
  (if (< (length buffer) num-elems)
      (let ([next-line (line-reader)])
        (if (void? next-line)
            (void)
          (get-words num-elems (append buffer (string-split next-line)) line-reader)))
    (begin
      (print (take buffer num-elems))
      (get-words num-elems (drop buffer num-elems) line-reader))))
      