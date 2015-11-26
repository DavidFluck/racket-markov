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

; This function takes a certain number of elements, a starting list, and a line-reader closure, and returns lists of
; words, with each list being num-elems in length.
(define (get-words num-elems buffer line-reader)
  (if (< (length buffer) num-elems)
      (let ([next-line (line-reader)])
        (if (void? next-line)
            (void)
          (get-words num-elems (append buffer (string-split next-line)) line-reader)))
    (begin
      (print (take buffer num-elems))
      (get-words num-elems (drop buffer num-elems) line-reader))))

(define (word-gen num-elems buffer line-reader)
  (generator ()
    (let get-words ([my-num-elems num-elems]
                    [my-buffer buffer]
                    [my-line-reader line-reader])
      (if (< (length my-buffer) my-num-elems)
          (let ([next-line (my-line-reader)])
            (if (void? next-line)
                (void)
                (get-words my-num-elems (append my-buffer (string-split next-line)) my-line-reader)))
          (begin
            (yield (take my-buffer my-num-elems))
            (get-words my-num-elems (drop my-buffer my-num-elems) line-reader))))))
