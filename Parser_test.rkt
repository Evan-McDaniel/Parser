#lang racket

(define (parse path)
  (define charList (string->list (file->string path)))
  ;(define tokens (scan (cons charList (list))))
  (car (program charList))
)

;car of return contains success or error cdr contains line number if error
(define (program charList)
  (if (car (linelist charList))
      (if (or (equal? (cdr (linelist charList)) null) (not (equal? (first (cdr (linelist charList))) "$$")))
          (error "did not end with $$")
          (cons "Correct" (list))
          )
      (error "Syntax error on line: " (cdr (linelist charList)))
      )
  )

(define (linelist charList)
  (if (or (equal? null (scan (cons charList (list)))) (equal? "$$" (first (scan (cons charList (list))))))
      (cons #t (scan (cons charList (list))))
      (if (not (equal? (member #\newline charList) #f))
          (if (car (line (get-line charList)))
              (linelist (rest (member #\newline charList)))
              (cons #f (get-line-number charList null))
              )
          (line (scan (cons charList (list))))
          )
      )
  )
(define (get-line charList)
  (scan (cons (take charList (index-of charList #\newline)) (list)))
  )

(define (get-line-number charLine number)
  (cond
   [(not (char-numeric? (first charLine))) "No line number"]
   [(not (char-numeric? (second charLine))) (list->string (append number (list (first charLine))))]
   [else (get-line-number (rest charLine) (append number (list (first charLine))))]
   )
  )

(define (line _line)
  (if (car (idx _line))
      (if (car (stmt(cdr (idx _line))))
          (line_tail (cdr (stmt(cdr (idx _line)))))
          (cons #f "error on line")
          )
      (cons #f "error on line")
      )
  )

(define (stmt _stmt)
  (cond
    [(and (equal? (first _stmt) "id") (equal? (second _stmt) "="))(expr (rest(rest _stmt)))]
    ;[(equal? (first _stmt) "if") (if (car (then (cdr (expr (rest _stmt))))) (stmt (cdr (then (cdr (expr (rest _stmt)))))) (cons #f "Error: then did not come after expr"))]
    [(equal? (first _stmt) "if") (stmt (cdr (then (cdr (expr (rest _stmt))))))]
    [(and (equal? (first _stmt) "read") (equal? (second _stmt) "id")) (cons #t (rest (rest _stmt)))]
    [(equal? (first _stmt) "write") (expr (rest _stmt))]
    [(and (equal? (first _stmt) "goto") (equal? (second _stmt) "idx")) (cons #t (rest (rest _stmt)))]
    [(and (equal? (first _stmt) "gosub") (equal? (second _stmt) "idx")) (cons #t (rest (rest _stmt)))]
    [(equal? (first _stmt) "return") (cons #t (rest _stmt))]
    )
  )

(define (then tlist)
  (if (equal? (first tlist) "then")
      (cons #t (rest tlist))
      (cons #f (rest tlist))
      )
  )

(define (expr _expr)
  (cond
    [(equal? (first _expr) "id") (etail (rest _expr))]
    [(equal? (first _expr) "num") (etail (rest _expr))]
    [(equal? (first _expr) "idx") (etail (rest _expr))]
    [(equal? (first _expr) "(") (if (and (car (expr (rest _expr))) equal? (last _expr) ")") (cons #t null) (cons #f "expression did not end with )"))]
    [else (cons #f (string-append "Syntax Error: expression cannot begin with: " (first _expr)))]
    )
  )

(define (etail _etail)
  (cond
    [(equal? _etail null) (cons #t null)]
    [(or (equal? (first _etail) "+") (equal? (first _etail) "-") (equal? (first _etail) "=")) (expr (rest _etail))]
    [else (cons #t _etail)]
    )
  )

(define (line_tail lineTail)
  (if (null? lineTail)
      (cons #t lineTail)
      (cond
        [(equal? (first lineTail) ":") (stmt(rest lineTail))]
        [else (cons #t lineTail)]
        )
      )
  )

(define (idx _line)
  (if (equal? (first _line) "idx")
      (cons #t (rest _line))
      (error (string-append "Syntax Error: line cannot begin with " (first _line)))
      )
  )

(define (scan pair)
  (define char (safe-first (car pair)))
  (if (equal? (car pair) null)
      (cdr pair)
      (cond
        [(equal? #\newline char) (scan(cons (rest (car pair)) (append (cdr pair) (list "[EOL]"))))]
        [(or (equal? char #\( ) (equal? char #\) ) (equal? char #\=) (equal? char #\:) ) (scan(cons (rest (car pair)) (append (cdr pair) (list (list->string (list char))))))]
        [(and (equal? char #\$) (equal? (second (car pair)) #\$)) (scan(cons (rest (rest (car pair))) (append (cdr pair) (list "$$"))))]
        [(char-whitespace? char) (scan (cons (rest (car pair)) (cdr pair)))]
        [(char-alphabetic? char) (scan (detect-alphabetic (car pair) (cdr pair) (list)))]
        ;[(char-numeric? char) (scan (cons (rest (car pair)) (append (cdr pair) (list (detect-numeric (first (car pair)))))))]
        [(or (char-numeric? char) (equal? #\- char) (equal? #\+ char)) (scan (cons (car (detect-numeric2 (car pair))) (append (cdr pair) (list (cdr (detect-numeric2 (car pair)))))))]
        ;[(or (equal? #\- char) (equal? #\+ char)) (scan (cons (rest (car pair)) (append (cdr pair) (list (detect-numsign (first (car pair)))))))]
        [else (error "invalid token in stream")]
        )
      )
  )

;found this function on https://docs.racket-lang.org/
(define (safe-first lst)
    (if (empty? lst)
        "nothing"
        (first lst)))

(define (detect-numsign char)
  (if (or (equal? #\- char) (equal? #\+ char))
      "numsign"
      (error"not a valid numsign")
      )
  )

(define (detect-numeric char)
  (if (equal? #\0 char)
      "digit"
      "nonzero_digit"
      )
  )
(define (detect-numeric2 charList)
  (if (or (equal? (first charList) #\-) (equal? (first charList) #\+))
      (if (char-whitespace? (second charList))
          (cons (rest charList) (string (first charList)))
          (if (or (equal? (cdr (detect-numeric2 (rest charList))) "digit") (equal? (cdr(detect-numeric2 (rest charList))) "nonzero_digit") (equal? (cdr(detect-numeric2 (rest charList))) "num") (equal? (cdr(detect-numeric2 (rest charList))) "idx"))
              (cons (car (detect-numeric2 (rest charList))) "num")
              (error "invalid symbol after - or +")
              )
          )
      (if (not (char-numeric? (first charList)))
          (error "number must have only digits")
          (if (not (char-numeric? (second charList)))
              (if (equal? (detect-numeric (first charList)) "nonzero_digit")
                  (cons (rest charList) "idx")
                  (cons (rest charList) "num")
                  )
              ;(cons (rest charList) (detect-numeric (first charList)))
              ;if detect numeric first == non zero -> idx else num
              (if (equal? "digit" (detect-numeric (first charList)))
                  (if (or (equal? "nonzero_digit" (detect-numeric (rest charList))) (equal? "digit" (detect-numeric (rest charList))) (equal? "num" (detect-numeric (rest charList))) (equal? "idx" (detect-numeric (rest charList))))
                      (cons (car (detect-numeric2 (rest charList))) "num")
                      (error "invalid character expected number")
                      )
                  (if (or (equal? (cdr (detect-numeric2 (rest charList))) "digit") (equal? (cdr (detect-numeric2 (rest charList))) "nonzero_digit") (equal? (cdr (detect-numeric2 (rest charList))) "idx") (equal? (cdr (detect-numeric2 (rest charList))) "num"))
                      (cons (car (detect-numeric2 (rest charList))) "idx")
                      (error"invalid number")
                      )
                  )
              )
          )
      )
  )

(define (detect-alphabetic charList tokenList token)
  (if (or (equal? charList null) (not (char-alphabetic? (first charList))))
      (if (detect-keyword (list->string token))
          (cons charList (append tokenList (list (list->string token))))
          (cons charList (append tokenList (list "id")))
          )
      (if (char-alphabetic? (first charList))
          (detect-alphabetic (rest charList) tokenList (append token (list (first charList))))
          (error "invalid character appearing after alphabetical")
          )
      )
  )

(define (detect-keyword token)
  (if (or (equal? token "if") (equal? token "then") (equal? token "read") (equal? token "write") (equal? token "goto") (equal? token "gosub") (equal? token "return"))
      #t
      #f
      )
  )