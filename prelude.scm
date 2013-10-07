;; Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(module prelude *
(import scheme chicken srfi-1 srfi-13 data-structures
        stdinerr extras)

;;; Get cdr of PAIR, or eval to #f if it's not a pair (CDr If Pair)
(define-syntax cdip
  (syntax-rules ()
    ((cdip expr)
     (let ((val expr))
       (if (pair? val)
           (cdr val)
           #f)))))

;;; Get car of PAIR, or eval to #f if it's not a pair (CAr If Pair)
(define-syntax caip
  (syntax-rules ()
    ((caip expr)
     (let ((val expr))
       (if (pair? val)
           (car val)
           #f)))))

;;; Sugar
(define-syntax nor
  (syntax-rules ()
    ((nor expr ...)
     (not (or expr ...)))))

(define-syntax nand
  (syntax-rules ()
    ((nand expr ...)
     (not (and expr ...)))))

;;; Accessor for values in a tree, as those returned by parse-json.
;;; Keys are strings (for use with alists) or numbers (for use with list-ref).
;;; Use multiple keys to go deeper into the tree. For example,
;;; (quick-ref tree "vids" 5 "url") is "url" in item 5 in "vids" in tree.
;;;
;;; Return values:
;;;   A list or cdr of a pair   (if the key exists and its value isn't #f)
;;;   #f                        (otherwise)
;;;
;;; Should produce an error if a negative list index is supplied.
(define (quick-ref obj . keys)
  (cond ((null? keys) obj)
        ((not (list? obj)) #f)
        ((and (number? (car keys))
              (list? obj)
              (< (car keys) (length obj)))
         (apply quick-ref (cons (list-ref obj (car keys)) (cdr keys))))
        ((or (symbol? (car keys))
             (string? (car keys)))
         (apply quick-ref (cons (cdip (assoc (car keys) (filter pair? obj)))
                                (cdr keys))))
        (else #f)))

;;; UNTESTED Accessor for values in an alist tree.
;;;
;;; Return values:
;;;   Any non-false value       (for any non-false existant value)
;;;   #f                        (otherwise)
(define (atree-ref tree . branches)
  (cond ((null? branches) tree)         ; Requested branch/leaf.
        ((not (pair? tree)) #f)         ; Leaf found, but expected branch.
        (else
         (apply atree-ref (cons (alist-ref (car branches) tree equal? #f)
                                (cdr branches))))))

;;; UNTESTED Updater for values in an alist tree.
;;;
;;; Return values:
;;;   An updated tree           (if the update was sucessful)
;;;   #f                        (if the update failed)
(define (atree-update tree value . branches)
  (cond ((or (null? branches)
             (not (list? tree))
             (not (every atom? branches)))
         #f)
        ((null? (cdr branches))
         (alist-update (car branches) value tree equal?))
        ((and (not (null? (cdr branches)))       ; Leaf of new branch requested
              (not (assoc (car branches) tree)))
         (apply atree-update (cons
                              (cons (cons (car branches) '()) tree)
                              (cons value branches))))
        (else
         (alist-update (car branches)
                       (apply atree-update
                         (cons (atree-ref tree (car branches))
                               (cons value (cdr branches))))
                       tree
                       equal?))))

;;; UNTESTED Deep merge of atrees. Overlays one atree on top of another.
;;; The overlay tree will replace any values present in the base.
(define (atree-merge base overlay)
  (cond ((null? overlay) base)
        ((not (pair? (car overlay))) #f)
        ((or (atom? (cdar overlay))
             (atom? (atree-ref base (caar overlay))))   ; #f if not found
         (atree-merge
          (atree-update base (cdar overlay) (caar overlay))
          (cdr overlay)))
        (else
         (atree-merge
          (atree-update base
                        (atree-merge (atree-ref base (caar overlay))
                                     (cdar overlay))
                        (caar overlay))
          (cdr overlay)))))

;;; UNTESTED Fold many atrees into one.
;;; Values present in the first supplied tree will replace those present
;;; in the second supplied tree, and so forth from left to right.
(define atree-fold
  (lambda trees
    (fold atree-merge '() trees)))

(define atree-fold-right
  (lambda trees
    (fold-right atree-merge '() trees)))

(define-syntax not-if
  (syntax-rules ()
    ((not-if test value)
     (if test (not value) value))))

(define (make-rnd-string len . tail)
  (let* ((str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (char (string-ref str (random (string-length str))))
         (appendix (if (null? tail) "" (car tail))))
    (if (> 1 len)
        appendix
        (make-rnd-string (- len 1) (conc char appendix)))))

;;; s/needle/replacement/g
(define (string-replace-every needle replacement string)
  (let loop ((rest string)
             (chunks '()))
    (let ((index (string-contains rest needle)))
      (cond
       ((= 0 (string-length needle))
        string)
       (index
        (loop (string-drop rest (+ index (string-length needle)))
              (cons replacement (cons (string-take rest index) chunks))))
       (else
        (apply conc (reverse (cons rest chunks))))))))

(define (char-changecase/swedish char upcase?)
  (let ((uppercase '(#\Å #\Ä #\Ö #\É #\Ü))
        (lowercase '(#\å #\ä #\ö #\é #\ü)))
    (let find ((needles (if upcase? lowercase uppercase))
               (replacements (if upcase? uppercase lowercase)))
      (cond
       ((null? needles) ((if upcase? char-upcase char-downcase) char))
       ((char=? (car needles) char) (car replacements))
       (else (find (cdr needles) (cdr replacements)))))))

(define (char-upcase/swedish char) (char-changecase/swedish char #t))
(define (char-downcase/swedish char) (char-changecase/swedish char #f))
(define (string-upcase/swedish str) (string-map char-upcase/swedish str))
(define (string-downcase/swedish str) (string-map char-downcase/swedish str))

(define (string-namecase/swedish str)
  (let loop ((rest (string->list str))
             (result '()))
    (cond
     ((null? rest)
      (string-replace-every
       " Von " " von "
       (string-replace-every
        " Af " " af "
        (reverse-list->string result))))
     ((or (null? result)
          (char=? (car result) #\space)
          (char=? (car result) #\-))
      (loop (cdr rest) (cons (char-upcase/swedish (car rest)) result)))
     (else
      (loop (cdr rest) (cons (char-downcase/swedish (car rest)) result))))))

(define (char-unswedish char)
  (let find ((needles (string->list "ÅÄÖÉåäöé"))
             (replacements (string->list "AAOEaaoe")))
    (cond
     ((null? needles) char)
     ((char=? (car needles) char) (car replacements))
     (else (find (cdr needles) (cdr replacements))))))

(define (string-unswedish str) (string-map char-unswedish str))

(define-syntax symbol-downcase
  (syntax-rules ()
    ((symbol-downcase symb)
     (string->symbol (string-downcase/swedish (symbol->string symb))))))

;;; Don't pretend to be json-ref
(define sxml-ref/proper
  (lambda args
    (apply quick-ref args)))

;;; While the json egg creates trees we sanitise into alists, the ssax
;;; egg creates trees where even key/value pairs are stored as proper
;;; lists (with a length of 2). If this sxml-ref procedure is used to
;;; refer to the cdr of such a pair, it returns the cadr instead of
;;; the cdr, which means it pretends it is accessing an alist. This
;;; way, the usage of it comes closer to that of json-ref.
(define (sxml-ref data . keys)
  (let ((val (apply sxml-ref/proper (cons data keys))))
    (if (and (list? val) (= 1 (length val)))
        (car val)
        val)))

(define sxml-element-eq?
  (lambda args
    (lambda (obj)
      (let loop ((keys args))
        (cond
         ((null? keys) #f)
         ((or (eq? (car keys) obj)
              (and (list? obj)
                   (eq? (car keys) (car obj)))))
         (else
          (loop (cdr keys))))))))

(define-syntax sxml-filter
  (syntax-rules ()
    ((sxml-filter sxml obj)
     (handle-exceptions exn #f
       (if (list? obj)
           (filter (apply sxml-element-eq? obj) sxml)
           (filter (sxml-element-eq? obj) sxml))))
    ((sxml-filter sxml elements ...)
     (handle-exceptions exn #f
       (filter (sxml-element-eq? elements ...) sxml)))))

(define (deep-filter pred tree)
  (let loop ((rest (filter pred tree))
             (result '()))
    (cond
     ((null? rest)
      (reverse result))
     ((list? (car rest))
      (loop (cdr rest)
            (cons (loop (filter pred (car rest)) '()) result)))
     (else
      (loop (cdr rest)
            (cons (car rest) result))))))

(define (deep-map proc list)
  (let loop ((rest (map proc list))
             (result '()))
    (cond
     ((null? rest)
      (reverse result))
     ((list? (car rest))
      (loop (cdr rest)
            (cons (loop (map proc (car rest)) '()) result)))
     (else
      (loop (cdr rest)
            (cons (car rest) result))))))

(define (alist-combine comb . tail)
  (fold (lambda (al1 al2)
          (cond
           ((not (every pair? al1))
            (error "Not proper alist" al1))
           ((not (every pair? al2))
            (error "Not proper alist" al2))
           (else
            (let loop ((rest al2)
                       (result al1))
              (cond
               ((null? rest) result)
               (else
                (let ((orig (alist-ref (caar rest) result)))
                  (loop (cdr rest)
                        (alist-update (caar rest)
                                      (if orig
                                          (comb orig (cdar rest))
                                          (cdar rest))
                                      result)))))))))
        '()
        tail))

(define-syntax string->number/default
  (syntax-rules ()
    ((string->number/default str def)
     (handle-exceptions exn def
       (string->number str)))))

(define-syntax string->number/false
  (syntax-rules ()
    ((string->number/false str)
     (string->number/default str #f))))

(define-syntax sxml-number
  (syntax-rules ()
    ((sxml-number d path ...)
     (string->number/default (sxml-ref d path ...) 0))))

(define-syntax sxml-number/f
  (syntax-rules ()
    ((sxml-number/f d path ...)
     (string->number/false (sxml-ref d path ...)))))

(define-syntax sxml-comma-percentage
  (syntax-rules ()
    ((sxml-comma-number d path ...)
     (/
      (string->number/default
       (string-map (lambda (c) (if (char=? c #\,) #\. c))
                   (sxml-ref d path ...))
       0)
      100))))

(define-syntax with-input-from-file*
  (syntax-rules ()
    ((with-input-from-file* file body ...)
     (with-input-from-file file
       (lambda ()
         body ...)))))

(define-syntax with-output-to-file*
  (syntax-rules ()
    ((with-output-to-file* file body ...)
     (with-output-to-file file
       (lambda ()
         body ...)))))

)
