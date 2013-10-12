;;; Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(use srfi-1 srfi-13 posix)

(include "stdinerr")
(import stdinerr)
(include "prelude")
(import prelude)

(define allow-expletives #t)

(define-record datum
  word
  possessive
  plural
  first-name-complete
  first-name-prefix
  first-name-suffix
  last-name-prefix
  last-name-suffix)

(define-record-printer (datum d port)
  (with-output-to-port port
    (lambda ()
      (print (conc "Word:                " (datum-word d) #\newline
                   "Possessive:          " (datum-possessive d) #\newline
                   "Plural:              " (datum-plural d) #\newline
                   "Complete first name? " (datum-first-name-complete d) #\newline
                   "First name prefix?   " (datum-first-name-prefix d) #\newline
                   "First name suffix?   " (datum-first-name-suffix d) #\newline
                   "Last name prefix?    " (datum-last-name-prefix d) #\newline
                   "Last name suffix?    " (datum-last-name-suffix d))))))

(define-record person
  full-name
  first-name
  middle-name
  last-name
  c/o
  street-name
  postal-number
  postal-address
  phone-number
  mobile-number
  email-address
  number)

(define-record-printer (person p port)
  (with-output-to-port port
    (lambda ()
      (print (conc "First Name:      " (person-first-name p) #\newline
                   (if (not (string=? (person-first-name p) (person-full-name p)))
                       (conc "Full Name:       " (person-full-name p) #\newline)
                       "")
                   (if (person-middle-name p)
                       (conc "Middle Name:     " (person-middle-name p) #\newline)
                       "")
                   "Last Name:       " (person-last-name p) #\newline
                   (if (person-c/o p)
                       (conc "c/o:             " (person-c/o p) #\newline)
                       "")
                   "Personal Number: " (person-number p) #\newline
                   "Street Name:     " (person-street-name p) #\newline
                   "Postal Address:  " (person-postal-address p) #\newline
                   "Postal Number:   " (person-postal-number p) #\newline
                   "Phone Number:    " (person-phone-number p) #\newline
                   "Mobile Number:   " (person-mobile-number p)
                   (if (person-email-address p)
                       (conc #\newline "Email Address:       " (person-email-address p))
                       ""))))))

(define (vowel? char)
  (string-any char "aAeEiIoOuUyYuUåÅäÄöÖüÜéÉáÁëË"))

(define (consonant? char)
  (string-any char "bBcCdDfFgGhHjJkKlLmMnNpPqQrRsStTvVwWxXzZ"))

(define (generic-possessive word)
  (cond
   ((or (string-suffix? "s" word)
        (string-suffix? "z" word)
        (string-suffix? "x" word))
    word)
   (else (conc word "s"))))

(define (generic-plural word)
  (cond
   ((string-suffix? "a" word)
    (conc (string-drop-right word 1) "or"))
   ((string-suffix? "e" word)
    (conc word "n"))
   ((or (string-suffix? "i" word)
        (string-suffix? "er" word)
        (let ((rev (reverse (string->list word))))
          (and (> (length rev) 1)
               (or (and (consonant? (car rev))
                        (char=? (car rev) (cadr rev)))
                   (and (consonant? (car rev))
                        (vowel? (cadr rev))
                        )))))
    word)
   ((or (string-suffix? "é" word)
        (string-suffix? "ar" word)
        (string-suffix? "ad" word)
        (string-suffix? "t" word)
        (string-suffix? "ld" word)
        (string-suffix? "ong" word)
        (string-suffix? "f" word))
    (conc word "er"))
   ((vowel? (car (reverse (string->list word))))
    (conc (string-drop-right word 1) "ar"))
   (else
    (conc word "ar"))))

(define (construct-datum w
                         #!key
                         (possessive #f)
                         (plural #f)
                         (first-name-complete #t)
                         (first-name-prefix #t)
                         (first-name-suffix #t)
                         (last-name-prefix #t)
                         (last-name-suffix #t)
                         (expletive #f)
                         (expand-minuses #f))
  (let* ((minus-prefix (and expand-minuses (string-prefix? "-" w)))
         (minus-suffix (and expand-minuses (string-suffix? "-" w)))
         (w2 (if minus-prefix (string-drop w 1) w))
         (word (string-downcase/swedish (if minus-suffix (string-drop-right w2 1) w2))))
    (if (and (not allow-expletives) expletive)
        #f
        (make-datum word
                    (or possessive (generic-possessive word))
                    (or plural (generic-plural word))
                    first-name-complete
                    (and first-name-prefix (not minus-prefix))
                    (and first-name-suffix (not minus-suffix))
                    (and last-name-prefix (not minus-prefix))
                    (and last-name-suffix (not minus-suffix))))))

(define (syllables word)
  (string-length (string-filter vowel? word)))

(define (syllable? word)
  (eq? 1 (syllables word)))

(define (datum-max-2-syllables datum)
  (> 3 (syllables (datum-word datum))))

(define (datum-max-1-syllable datum)
  (syllable? (datum-word datum)))

(define (datum-first-name-one-syllable-prefix datum)
  (and (syllable? (datum-word datum))
       (datum-first-name-prefix datum)))

(define (datum-first-name-two-syllable-prefix datum)
  (and (= 2 (syllables (datum-word datum)))
       (datum-first-name-prefix datum)))

(define (datum-first-name-one-syllable-suffix datum)
  (and (syllable? (datum-word datum))
       (datum-first-name-suffix datum)))

(define (datum-last-name-one-syllable-suffix datum)
  (and (syllable? (datum-word datum))
       (datum-last-name-suffix datum)))

(define (datum-first-name-two-syllable-suffix datum)
  (and (= 2 (syllables (datum-word datum)))
       (datum-first-name-suffix datum)))

(define (datum-first-name-one-syllable-complete datum)
  (and (syllable? (datum-word datum))
       (datum-first-name-complete datum)))

(define (parse-datum d . construct-opts)
  (cond
   ((symbol? d)
    (apply construct-datum (cons (symbol->string d) construct-opts)))
   ((string? d)
    (apply construct-datum (cons d construct-opts)))
   ((list? d)
    (apply construct-datum (append d construct-opts)))
   (else (error "Neither string, symbol nor list in datum" d))))

(include "data/first-only.scm")
(include "data/generic-components.scm")
(include "data/last-components.scm")

(define datum
  (filter datum?
          (append (map parse-datum
                       (filter (lambda (sym)
                                 (syllable? (symbol->string sym)))
                               data-first-only))
                  (map parse-datum
                       (filter (lambda (sym)
                                 (= 2 (syllables (symbol->string sym))))
                               data-first-only))
                  (map (lambda (d)
                         (parse-datum d
                                      first-name-prefix: #f
                                      first-name-suffix: #f
                                      last-name-prefix: #f
                                      last-name-suffix: #f))
                       (filter (lambda (sym)
                                 (and (< 2 (syllables (symbol->string sym)))))
                               data-first-only))
                  (map (lambda (d)
                         (parse-datum d
                                      first-name-complete: #f
                                      expand-minuses: #t))
                       data-generic-components)
                  (map (lambda (d)
                         (parse-datum d
                                      expand-minuses: #t
                                      first-name-complete: #f
                                      first-name-prefix: #f
                                      first-name-suffix: #f))
                       data-last-components))))

(define (pick-datum #!optional (condition #f))
  (let* ((lst (if condition
                  (filter condition datum)
                  datum))
         (len (length lst)))
    (if (< 0 len)
        (list-ref lst (random len))
        #f)))

(define (expand-combination combination)
  (let loop ((n (car combination))
             (value '()))
    (cond
     ((= 0 n) value)
     (else (loop (- n 1) (cons (cdr combination) value))))))

(define (pick-combination combinations)
  (let* ((combination (apply append (map expand-combination combinations)))
         (len (length combination)))
    (list-ref combination (random len))))

(define (without-vowel-consonant-suffix word)
  (let ((backwards (reverse (string->list word))))
    (if (and (< 1 (length backwards))
             (consonant? (car backwards))
             (vowel? (cadr backwards)))
        (string-drop-right word 2)
        word)))

(define (make-first-name #!key
                         (accessor datum-word)
                         (last-accessor datum-word)
                         (override-combination #f)
                         (filter-dash-combinations #f))
  (let* ((custom-filter (lambda (combos)
                          (if filter-dash-combinations
                              (filter (lambda (lis) (not (member "-" lis equal?))) combos)
                              combos)))
         (maybe-add-vowel (lambda (word)
                            (let ((should-add (> 1 (random 6)))
                                  (endings (append (list "ina" "ita" "ia" "ie" "ete" "ela")
                                                   (map ->string (string->list "eeaaaaaaaiyy")))))
                              (if (and should-add
                                       (consonant? (car (reverse (string->list word)))))
                                  (conc (without-vowel-consonant-suffix word)
                                        (list-ref endings (random (length endings))))
                                  word))))
         (combination (if override-combination
                          override-combination
                          (pick-combination (custom-filter
                                             '((180 datum-first-name-complete)
                                               (30 datum-first-name-one-syllable-prefix
                                                   datum-first-name-one-syllable-suffix)
                                               (1 datum-first-name-two-syllable-prefix
                                                  datum-first-name-one-syllable-suffix)
                                               (15 datum-first-name-one-syllable-prefix
                                                   datum-first-name-suffix)
                                               (1 datum-first-name-one-syllable-complete
                                                  "-"
                                                  datum-first-name-complete)
                                               (15 datum-first-name-one-syllable-complete
                                                   "-"
                                                   datum-first-name-two-syllable-suffix)
                                               (1 datum-first-name-one-syllable-prefix
                                                  "-"
                                                  datum-first-name-one-syllable-suffix)
                                               (1 datum-first-name-one-syllable-prefix
                                                  "-"
                                                  datum-first-name-one-syllable-prefix
                                                  datum-first-name-one-syllable-suffix)
                                               (2 datum-first-name-prefix
                                                  "-"
                                                  datum-first-name-suffix)))))))
    (let loop ((rest combination)
               (result '()))
      (let ((this-accessor (if (and (not (null? rest))
                                    (null? (cdr rest)))
                               last-accessor
                               accessor)))
        (cond
         ((null? rest)
          (string-namecase/swedish
           (remove-triplets
            (remove-bad-combinations
             (if last-accessor
                 (apply conc (reverse result))
                 (maybe-add-vowel (apply conc (reverse result))))))))
         ((string? (car rest))
          (loop (cdr rest) (cons (car rest) result)))
         ((list? (car rest))
          (loop (cons (list-ref (car rest) (random (length (car rest)))) rest) result))
         (else
          (loop (cdr rest)
                (cons (this-accessor (pick-datum (eval (car rest)))) result))))))))

(define (make-last-name #!key (override-combination #f) (no-common #f))
  (let* ((combination (if override-combination
                          override-combination
                          (pick-combination
                           `((80 datum-last-name-prefix
                                 datum-last-name-suffix)
                             (80 datum-last-name-prefix
                                 datum-last-name-one-syllable-suffix)
                             (20 make-one-syllable-first-name
                                 datum-last-name-suffix)
                             (10 make-two-syllable-first-name
                                 datum-last-name-suffix)
                             (,(if no-common 0 2)
                              "von " datum-last-name-prefix datum-first-name-prefix)
                             (,(if no-common 0 4)
                              "af " datum-last-name-prefix datum-first-name-prefix)
                             (,(if no-common 0 250)
                              make-possessive-first-name
                              ((70 "son")
                               (10 "on")
                               (5 "en")
                               (2 "dotter")))
                             (,(if no-common 0 200)
                              datum-last-name-prefix
                              ((15 "quist")
                               (15 "qvist")
                               (15 "kvist")
                               (2 "én")
                               (5 "stam")
                               (15 "fors")
                               (10 "löv")
                               (5 "lund")
                               (5 "löf")
                               (15 "blad")
                               (15 "ström")
                               (10 "bäck")
                               (1 "beck")
                               (5 "mark")
                               (5 "strand")
                               (10 "rot")
                               (3 "bark")
                               (5 "skog")
                               (10 "sten")
                               (2 "sand")
                               (1 "stein")
                               (10 "sjö")
                               (30 "gren")
                               (30 "berg")))
                             (20 datum-last-name-prefix)
                             (20 datum-last-name-suffix)
                             (5 datum-possessive datum-last-name-prefix)
                             (5 datum-possessive datum-last-name-suffix)
                             (30 make-one-syllable-first-name)
                             (30 make-two-syllable-first-name)
                             (5 datum-possessive datum-last-name-prefix datum-last-name-suffix)
                             (5 make-possessive-first-name " " datum-last-name-suffix)
                             (5 datum-possessive
                                datum-first-name-complete
                                datum-last-name-suffix))))))
    (let loop ((rest combination)
               (result '()))
      (cond
       ((null? rest)
        (remove-triplets
         (remove-bad-combinations
          (string-namecase/swedish (apply conc (reverse result))))))
       ((string? (car rest))
        (loop (cdr rest) (cons (car rest)
                               result)))
       ((and (list? (car rest))
             (list? (caar rest)))
        (loop (append (pick-combination (car rest)) (cdr rest))
              result))
       ((list? (car rest))
        (loop (cons (list-ref (car rest) (random (length (car rest))))
                    (cdr rest))
              result))
       ((eq? 'datum-possessive (car rest))
        (loop (cddr rest)
              (cons (datum-possessive (pick-datum (eval (cadr rest))))
                    result)))
       ((eq? 'datum-plural (car rest))
        (loop (cddr rest)
              (cons (datum-plural (pick-datum (eval (cadr rest))))
                    result)))
       ((eq? 'datum-possessive-plural (car rest))
        (loop (cddr rest)
              (cons (generic-plural (datum-possessive (pick-datum (eval (cadr rest)))))
                    result)))
       ((eq? 'make-possessive-first-name (car rest))
        (loop (cdr rest)
              (cons (make-first-name filter-dash-combinations: #t
                                     last-accessor: datum-possessive) result)))
       ((eq? 'make-one-syllable-first-name (car rest))
        (loop (cdr rest)
              (cons (make-first-name override-combination: '(datum-first-name-one-syllable-prefix))
                    result)))
       ((eq? 'make-two-syllable-first-name (car rest))
        (loop (cdr rest)
              (cons (make-first-name override-combination: '(datum-first-name-two-syllable-prefix))
                    result)))
       ((eq? 'make-first-name (car rest))
        (loop (cdr rest)
              (cons (make-first-name) result)))
       ((eq? 'make-last-name (car rest))
        (loop (cdr rest)
              (cons (make-last-name) result)))
       (else
        (let ((ref (if (= 0 (random 3)) datum-plural datum-word)))
          (loop (cdr rest)
                (cons (ref (pick-datum (eval (car rest)))) result))))))))

(define (remove-triplets word)
  (let loop ((rest (string->list word))
             (result '()))
    (cond
     ((null? rest)
      (apply conc (reverse result)))
     ((null? (cdr rest)) (apply conc (reverse (cons (car rest) result))))
     ((and (not (null? (cddr rest)))
           (char=? (car rest) (cadr rest))
           (char=? (car rest) (caddr rest)))
      (loop (cdr rest) result))
     (else
      (loop (cdr rest) (cons (car rest) result))))))

(define (remove-bad-combinations word)
  (let loop ((rest (string->list word))
             (result '()))
    (cond
     ((null? rest)
      (apply conc (reverse result)))
     ((null? (cdr rest))
      (apply conc (reverse (cons (car rest) result))))
     ((and (char=? (car rest) (cadr rest))
           (member (car rest) (string->list "hj")))
      (loop (cdr rest) result))
     ((and (not (null? (cddr rest)))
           (or (char=? (car rest) #\m)
               (char=? (car rest) #\n))
           (char=? (car rest) (cadr rest))
           (consonant? (caddr rest)))
      (loop (cdr rest) result))
     (else
      (loop (cdr rest) (cons (car rest) result))))))

(define (make-name)
  (let ((extra-first-name (if (< (random 40) 1) (make-first-name) #f))
        (extra-last-name (if (< (random 30) 1) (make-last-name) #f)))
    (string-namecase/swedish
     (remove-triplets
      (remove-bad-combinations
       (string-intersperse (filter identity (list (make-first-name)
                                                  extra-first-name
                                                  extra-last-name
                                                  (make-last-name)))))))))

(define (make-registry-style-name #!key (mark-proc string-upcase/swedish))
  (let* ((mp (if mark-proc mark-proc identity))
         (spoken-name (make-first-name))
         (generate-names (lambda (number-of-names)
                           (let ((split (+ 1 (random number-of-names))))
                             (if (= split 1)
                                 (mp spoken-name)
                                 (let loop ((n number-of-names)
                                            (result '()))
                                   (cond
                                    ((= n 0) (string-intersperse (reverse result)))
                                    ((= n split)
                                     (loop (- n 1)
                                           (cons (mp spoken-name) result)))
                                    (else (loop (- n 1)
                                                (cons (make-first-name) result)))))))))
         (last-name (conc (make-last-name)
                          (if (= 0 (random 30))
                              (conc " " (make-last-name))
                              ""))))
    (conc (generate-names (+ 1 (random 5))) " " (mp last-name))))

(define (make-c/o)
  (conc "c/o " (if (= 0 (random 7))
                   (make-name)
                   (make-last-name))))

(define (make-place-name #!key
                         (override-combination #f)
                         (make-prefix #f)
                         (bigger-profile #f))
  (let ((combination (if override-combination
                         override-combination
                         (pick-combination `((5 ((3 datum
                                                    possible-infix
                                                    last-datum
                                                    adjust-case)
                                                 (3 ((1 shorter-datum short-datum adjust-case)
                                                     (1 short-datum shorter-datum adjust-case)))))
                                             (80 ((1 ((1 "Övre")
                                                      (1 "Nedre")
                                                      (1 "Bortre")
                                                      (1 "Större")
                                                      (1 "Södra")
                                                      (1 "Norra")
                                                      (1 "Västra")
                                                      (1 "Östra")
                                                      (1 "Stora")
                                                      (1 "Lilla")
                                                      (1 "Mindre")
                                                      (1 "Lägre")
                                                      (1 "Högre"))
                                                     " ")
                                                  (70 ""))
                                                 ((90 last-name-no-common)
                                                  (90 first-name-no-dashes)
                                                  (5 datum datum adjust-case)
                                                  (18 short-datum shorter-datum adjust-case)
                                                  (9 shorter-datum short-datum adjust-case)
                                                  (20 place-prefix)
                                                  (4 shorter-datum shorter-datum adjust-case)
                                                  (150 ((1 shorter-datum)
                                                        (1 short-datum))
                                                       adjust-case)
                                                  (45 datum adjust-case))
                                                 possible-infix
                                                 ((,(if bigger-profile 1 3)
                                                   ,(if make-prefix "gårds" "gård"))
                                                  (,(if bigger-profile 0 1)
                                                   ,(if make-prefix "gårda" "gården"))
                                                  (1 "hus")
                                                  (1 "tull")
                                                  (1 ,(if make-prefix "källe" "källa"))
                                                  (1 ,(if make-prefix "tjärne" "tjärn"))
                                                  (1 ,(if make-prefix "bygds" "bygd"))
                                                  (1 "mo")
                                                  (3 "bo")
                                                  (1 "brona")
                                                  (1 ,(if make-prefix "vads" "vad"))
                                                  (1 ,(if make-prefix "vadar" "vadet"))
                                                  (6 "sjö")
                                                  (3 "ö")
                                                  (1 ,(if make-prefix "öa" "ön"))
                                                  (6 ,(if make-prefix "bergs" "berg"))
                                                  (6 ,(if make-prefix "borgs" "borg"))
                                                  (1 "fors")
                                                  (2 ,(if make-prefix "ströms" "ström"))
                                                  (2 "berga")
                                                  (1 "ro")
                                                  (1 ,(if make-prefix "hytte" "hyttan"))
                                                  (1 ,(if make-prefix "sanda" "sand"))
                                                  (1 ,(if make-prefix "arps" "arp"))
                                                  (2 ,(if make-prefix "strands" "strand"))
                                                  (1 ,(if make-prefix "hytte" "hyttan"))
                                                  (1 "vara")
                                                  (1 "länge")
                                                  (1 ,(if make-prefix "vika" "viken"))
                                                  (1 ,(if make-prefix "skärs" "skär"))
                                                  (2 ,(if make-prefix "viks" "vik"))
                                                  (1 ,(if make-prefix "kulle" "kullen"))
                                                  (3 ,(if make-prefix "kyrko" "kyrka"))
                                                  (1 ,(if make-prefix
                                                          (car
                                                           (pick-combination
                                                            '((1 "hulte") (1 "hults") (1 "hulta"))))
                                                          "hult"))
                                                  (3 ,(if make-prefix "fjälls" "fjäll"))
                                                  (1 ,(if make-prefix "fjälla" "fjällen"))
                                                  (1 ,(if make-prefix "jokks" "jokk"))
                                                  (1 ,(if make-prefix "mokks" "mokk"))
                                                  (1 ,(if make-prefix "umans" "uman"))
                                                  (1 ,(if make-prefix "sågs" "såg"))
                                                  (1 "valla")
                                                  (2 ,(if make-prefix "valla" "vall"))
                                                  (3 ,(if make-prefix "åkers" "åker"))
                                                  (2 ,(if make-prefix "dals" "dal"))
                                                  (1 "dala")
                                                  (2 ,(if make-prefix "boda" "bodarna"))
                                                  (1 ,(if make-prefix "stigs" "stigarna"))
                                                  (1 "måla")
                                                  (1 ,(if make-prefix "brunns" "brunn"))
                                                  (1 "sta")
                                                  (3 ,(if make-prefix "lunds" "lund"))
                                                  (2 "ås")
                                                  (1 "åsen")
                                                  (1 ,(if make-prefix "marka" "mark"))
                                                  (1 "lycke")
                                                  (,(if bigger-profile 0 1)
                                                   ,(if make-prefix "lycko" "lyckan"))
                                                  (1 ,(if make-prefix "bols" "bolet"))
                                                  (6 ,(if make-prefix "sunda" "sund"))
                                                  (1 ,(if make-prefix "grunds" "grund"))
                                                  (4 "sala")
                                                  (6 ,(if make-prefix "stads" "stad"))
                                                  (1 ,(if make-prefix "bola" "bol"))
                                                  (1 ,(if make-prefix "kroks" "krok"))
                                                  (6 "bro")
                                                  (6 ,(if make-prefix "köpings" "köping"))
                                                  (6 ,(if make-prefix "bruks" "bruk"))
                                                  (6 ,(if make-prefix "hammars" "hammar"))
                                                  (1 ,(if make-prefix "brotte" "brott"))
                                                  (4 "by")
                                                  (5 ,(if make-prefix "skogs" "skog"))
                                                  (5 ,(if make-prefix "torps" "torp"))
                                                  (1 ,(if make-prefix "vattne" "vattnet"))
                                                  (1 ,(if make-prefix "vattna" "vatten"))
                                                  (,(if bigger-profile 0 1)
                                                   ,(if make-prefix "torpa" "torpet"))
                                                  (1 ,(if make-prefix "nytto" "nytta"))
                                                  (1 "gräns")))
                                             (,(if make-prefix 0 5)
                                              ((3 last-name-no-common)
                                               (3 first-name)
                                               (1 datum datum adjust-case)
                                               (3 datum adjust-case)))
                                             (,(if (or make-prefix bigger-profile) 0 1)
                                              ((1 first-name) (1 possessive-first-name)
                                               (1 last-name) (1 possessive-last-name))
                                              " och "
                                              ((1 possessive-first-name) (1 possessive-last-name))
                                              " "
                                              datum-last-name-suffix)
                                             (,(if (or make-prefix bigger-profile) 0 5)
                                              ((1 possessive-name)
                                               (1 possessive-first-name-no-dashes)
                                               (1 possessive-datum)
                                               (1 possessive-double-datum)
                                               (1 possessive-last-name))
                                              " "
                                              ((20 "sund")
                                               (1 "slott")
                                               (3 "herrgård")
                                               (1 "fästning")
                                               (1 "borg")
                                               (5 "backe")
                                               (20 "stugby")
                                               (10 "kyrkby")
                                               (40 "kyrka")
                                               (60 "gård")
                                               (5 "träsk")
                                               (5 "bo")
                                               (10 "ås")
                                               (50 "sjö")
                                               (40 "berg")
                                               (40 "bruk")
                                               (30 "skog")
                                               (5 "grav")
                                               (1 "gräns")
                                               (30 "by")
                                               (10 "lycka"))))))))
    (let loop ((rest combination)
               (result '()))
      (cond
       ((null? rest)
        (remove-triplets
         (remove-bad-combinations
          (apply conc (reverse result)))))
       ((not (car rest))
        (loop (cdr rest) result))
       ((and (list? (car rest))
             (list? (caar rest)))
        (loop (append (pick-combination (car rest)) (cdr rest)) result))
       ((list? (car rest))
        (loop (cons (list-ref (car rest) (random (length (car rest))))
                    (cdr rest))
              result))
       ((eq? 'possible-infix (car rest))
        (if (= 0 (random 10))
            (loop (cons 'infix (cdr rest)) result)
            (loop (cdr rest) result)))
       ((eq? 'infix (car rest))
        (if (or (vowel? (string-ref (car result) (- (string-length (car result)) 1)))
                (string-suffix? "s" (car result)))
            (loop (cdr rest) result)
            (loop (cdr rest)
                  (cons (car (pick-combination '((1 "s") (4 "a") (8 "e") (2 "i") (1 "o") (1 "u"))))
                        result))))
       ((eq? 'adjust-case (car rest))
        (loop (cdr rest)
              (list (string-namecase/swedish (apply conc (reverse result))))))
       ((eq? 'place-prefix (car rest))
        (loop (cdr rest) (cons (make-place-name make-prefix: #t) result)))
       ((eq? 'possessive-datum (car rest))
        (loop (cdr rest) (cons (string-namecase/swedish (datum-possessive (pick-datum))) result)))
       ((eq? 'possessive-double-datum (car rest))
        (loop (cdr rest) (cons (string-namecase/swedish (conc (datum-word (pick-datum))
                                                              (datum-possessive (pick-datum))))
                               result)))
       ((eq? 'possessive-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-name)) result)))
       ((eq? 'possessive-first-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-first-name)) result)))
       ((eq? 'possessive-first-name-no-dashes (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-first-name filter-dash-combinations: #t))
                               result)))
       ((eq? 'possessive-last-name-no-common (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-last-name no-common: #t)) result)))
       ((eq? 'possessive-last-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-last-name)) result)))
       ((eq? 'last-name (car rest))
        (loop (cdr rest) (cons (make-last-name) result)))
       ((eq? 'last-name-no-common (car rest))
        (loop (cdr rest) (cons (make-last-name no-common: #t) result)))
       ((eq? 'last-name-no-common (car rest))
        (loop (cdr rest) (cons (make-last-name no-common: #t) result)))
       ((eq? 'name (car rest))
        (loop (cdr rest) (cons (make-name) result)))
       ((eq? 'first-name (car rest))
        (loop (cdr rest) (cons (make-first-name) result)))
       ((eq? 'first-name-no-dashes (car rest))
        (loop (cdr rest) (cons (make-first-name filter-dash-combinations: #t) result)))
       ((eq? 'last-datum (car rest))
        (if (not make-prefix)
            (loop (cons 'datum (cdr rest)) result)
            (loop (cons 'datum (cons 'infix (cdr rest))) result)))
       ((eq? 'datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish ((let ((n (random 12)))
                                                (cond
                                                 ((< n 4) datum-word)
                                                 ((< n 8) datum-possessive)
                                                 (else datum-plural)))
                                              (pick-datum)))
                    result)))
       ((eq? 'short-datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish
                     ((let ((n (random 12)))
                        (cond
                         ((< n 4) datum-word)
                         ((< n 8) datum-possessive)
                         (else datum-plural)))
                      (pick-datum datum-max-2-syllables)))
                    result)))
       ((eq? 'shorter-datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish
                     ((let ((n (random 12)))
                        (cond
                         ((< n 4) datum-word)
                         ((< n 8) datum-possessive)
                         (else datum-plural)))
                      (pick-datum datum-max-1-syllable)))
                    result)))
       ((string? (car rest))
        (loop (cdr rest) (cons (car rest) result)))
       (else
        (loop (cdr rest)
              (cons (datum-word (pick-datum (eval (car rest)))) result)))))))

(define (make-postal-address)
  (make-place-name bigger-profile: #t))

(define (make-street-number)
  (let ((combination (pick-combination '((40 30)
                                         (15 60)
                                         (5 50 ("" " ") letter)
                                         (2 80 letter " "
                                            ((5 3)
                                             (2 6)
                                             (1 10))
                                            ((5 "tr") (1 " tr")))
                                         (1 150 letter " "
                                            ((5 3)
                                             (2 6)
                                             (1 10))
                                            ((5 "tr") (1 " tr")))))))
    (let loop ((rest combination)
               (result '()))
      (cond
       ((null? rest)
        (apply conc (reverse result)))
       ((and (list? (car rest))
             (list? (caar rest)))
        (loop (append (pick-combination (car rest)) (cdr rest))
              result))
       ((list? (car rest))
        (loop (cons (list-ref (car rest) (random (length (car rest))))
                    (cdr rest))
              result))
       ((eq? 'letter (car rest))
        (loop (cdr rest)
              (cons (let loop ((rest (string->list "ABCDEFGHIJKLMNOPQRSTUV"))
                               (result '()))
                      (cond
                       ((null? rest)
                        (let ((result (apply conc result)))
                          (string-ref result (random (string-length result)))))
                       (else
                        (loop (cdr rest)
                              (cons (string-pad "" (* (length rest) (length rest)) (car rest))
                                    result)))))
                    result)))
       ((number? (car rest))
        (loop (cdr rest)
              (cons (number->string (+ 1 (random (car rest))))
                    result)))
       ((string? (car rest))
        (loop (cdr rest) (cons (car rest) result)))
       (else
        (loop (cdr rest)
              (cons (datum-word (pick-datum (eval (car rest)))) result)))))))

(define (make-street-name #!key (with-number #f) (with-number-marker #f))
  (let* ((with-number (or with-number with-number-marker))
         (country-side-number (number->string
                               (+ (or (and (= 0 (random 5))
                                           (random 10))
                                      0)
                                  (or (and (= 0 (random 2))
                                           5)
                                      0)
                                  (* 10 (random 100)))))
         (combination (pick-combination `((1 ((1 datum)
                                              (1 post-address))
                                             " "
                                             ,country-side-number)
                                          (1 ((1 datum datum adjust-case)
                                              (1 post-address))
                                             ,(if (= 0 (random 3))
                                                  ""
                                                  (conc " " country-side-number)))
                                          (5 ((10 possessive-name)
                                              (5 possessive-first-name)
                                              (5 possessive-last-name))
                                             " "
                                             ((7 "väg")
                                              (5 "gata")
                                              (5 "stig")
                                              (3 "gång")
                                              (2 "gränd")
                                              (3 "allé")
                                              (2 "backe")
                                              (1 "torg"))
                                             ,(if with-number " " "")
                                             ,(if with-number 'street-number ""))
                                          (60 ((5 post-address-prefix)
                                               (15 datum)
                                               (1 shorter-datum shorter-datum short-datum
                                                  adjust-case)
                                               (3 shorter-datum shorter-datum adjust-case)
                                               (2 short-datum shorter-datum adjust-case))
                                              ((10 "vägen")
                                               (7 "stigen")
                                               (4 "backe")
                                               (2 "backen")
                                               (4 "gatan")
                                               (2 "allén")
                                               (5 "svängen")
                                               (1 "torget")
                                               (1 "plan")
                                               (2 "lund")
                                               (2 "lunden")
                                               (10 "gången"))
                                              ,(if with-number " " "")
                                              ,(if with-number 'street-number ""))
                                          (,(if with-number 2 10)
                                           ((1 post-address-prefix)
                                            (1 first-name))
                                           ((1 " gård")
                                            (1 " by")
                                            (1 "by")
                                            (8 "torp")))))))
    (let loop ((rest combination)
               (result '()))
      (cond
       ((null? rest)
        (remove-triplets
         (remove-bad-combinations
          (apply conc (reverse result)))))
       ((and (list? (car rest))
             (list? (caar rest)))
        (loop (append (pick-combination (car rest)) (cdr rest))
              result))
       ((not (car rest))
        (loop (cdr rest) result))
       ((number? (car rest))
        (loop (cdr rest) (cons (number->string (car rest)) result)))
       ((eq? 'adjust-case (car rest))
        (loop (cdr rest)
              (list (string-namecase/swedish (apply conc (reverse result))))))
       ((list? (car rest))
        (loop (cons (list-ref (car rest) (random (length (car rest))))
                    (cdr rest))
              result))
       ((eq? 'street-number (car rest))
        (loop (cdr rest) (cons (if with-number-marker
                                   "#"
                                   (make-street-number))
                               result)))
       ((eq? 'post-address (car rest))
        (loop (cdr rest) (cons (make-place-name) result)))
       ((eq? 'post-address-prefix (car rest))
        (loop (cdr rest) (cons (make-place-name make-prefix: #t) result)))
       ((eq? 'possessive-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-name)) result)))
       ((eq? 'possessive-first-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-first-name)) result)))
       ((eq? 'possessive-last-name (car rest))
        (loop (cdr rest) (cons (generic-possessive (make-last-name)) result)))
       ((eq? 'last-name (car rest))
        (loop (cdr rest) (cons (make-last-name) result)))
       ((eq? 'name (car rest))
        (loop (cdr rest) (cons (make-name) result)))
       ((eq? 'first-name (car rest))
        (loop (cdr rest) (cons (make-first-name) result)))
       ((eq? 'datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish ((let ((n (random 10)))
                                                (cond
                                                 ((< n 4) datum-word)
                                                 ((< n 8) datum-possessive)
                                                 (else datum-plural)))
                                              (pick-datum)))
                    result)))
       ((eq? 'short-datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish
                     ((let ((n (random 12)))
                        (cond
                         ((< n 4) datum-word)
                         ((< n 8) datum-possessive)
                         (else datum-plural)))
                      (pick-datum datum-max-2-syllables)))
                    result)))
       ((eq? 'shorter-datum (car rest))
        (loop (cdr rest)
              (cons (string-namecase/swedish
                     ((let ((n (random 12)))
                        (cond
                         ((< n 4) datum-word)
                         ((< n 8) datum-possessive)
                         (else datum-plural)))
                      (pick-datum datum-max-1-syllable)))
                    result)))
       ((string? (car rest))
        (loop (cdr rest) (cons (car rest) result)))
       (else
        (loop (cdr rest)
              (cons (datum-word (pick-datum (eval (car rest)))) result)))))))

(define (make-postal-number)
  (conc (+ 100 (random 900)) " " (string-pad (number->string (random 100)) 2 #\0)))

(define (make-social-security-crc year month date num1 num2 num3)
  (let ((mult (lambda (num)
                (let calc ((n (* 2 num)))
                  (if (> n 9)
                      (calc (+ (quotient n 10)
                               (modulo n 10)))
                      n))))
        (y1 (quotient year 10))
        (y2 (modulo year 10))
        (m1 (quotient month 10))
        (m2 (modulo month 10))
        (d1 (quotient date 10))
        (d2 (modulo date 10)))
    (modulo (- 10
               (modulo (+ (mult y1) y2
                          (mult m1) m2
                          (mult d1) d2
                          (mult num1) num2 (mult num3))
                       10))
            10)))

(define (make-social-security-number #!key (full-year #t))
  (let* ((2pad (lambda (n) (string-pad (number->string n) 2 #\0)))
         (4pad (lambda (n) (string-pad (number->string n) 4 #\0)))
         (age-curve (list 0
                          1 1 1 1 1
                          2 2 2 2 2 2 2 2 2
                          3 3 3 3 3 3 3 3 3
                          4 4 4 4 4 4 4 4 4
                          5 5 5 5 5 5
                          6 6 6 6
                          7
                          8
                          9))
         (age-category (list-ref age-curve (random (length age-curve))))
         (year (- (+ 1900 (vector-ref (seconds->local-time (current-seconds)) 5))
                  (+ (* 10 age-category) (random 10))))
         (no-century (modulo year 100))
         (date-point (+ (current-seconds) (random 252460800)))
         (month (+ 1 (vector-ref (seconds->local-time date-point) 4)))
         (day (vector-ref (seconds->local-time date-point) 3))
         (num1 (random 10))
         (num2 (random 10))
         (num3 (random 10))
         (num4 (make-social-security-crc no-century month day num1 num2 num3)))
    (conc (if full-year
              (4pad year)
              (2pad year))
          (2pad month)
          (2pad day)
          "-"
          num1
          num2
          num3
          num4)))

(define (make-phone-number #!key (force-mobile #f))
  (let* ((rikt1 "0")
         (rikt2 (if force-mobile
                    "7"
                    (number->string (+ 1 (random 9)))))
         (is-mobile (string=? "7" rikt2))
         (rikt3 (number->string (random 10)))
         (rikt4 (if (or (= 0 (random 5))
                        (string=? "7" rikt2))
                    (number->string (random 10))
                    ""))
         (rikt (conc rikt1 rikt2 rikt3 rikt4))
         (rest-len (- (if is-mobile
                          10
                          (+ 9 (random 3)))
                      (string-length rikt))))
    (let loop ((n rest-len)
               (result (list "-" rikt)))
      (cond
       ((= 0 n) (apply conc (reverse result)))
       (else (loop (- n 1) (cons (random 10) result)))))))

(define (mobile-number? str)
  (string-prefix? "07" str))

(define (make-address #!key (additional-data #f))
  (let ((c/o (and (= 0 (random 40)) (make-c/o))))
    (conc (if additional-data
              (make-registry-style-name)
              (make-name))
          (if additional-data #\newline "")
          (if c/o
              (conc (if additional-data "" " ") (conc c/o #\newline))
              (if (not additional-data)
                  #\newline
                  ""))
          (if additional-data (conc (make-social-security-number) #\newline) "")
          (if (= 0 (random 30))
              ""
              (conc (make-street-name with-number: #t) #\newline))
          (make-postal-number) " " (make-postal-address) #\newline
          (if additional-data
              (let ((phone (make-phone-number)))
                (conc
                 phone " / "
                 (if (string-prefix? "07" phone)
                     phone
                     (make-phone-number force-mobile: #t))
                 #\newline))
              ""))))

(define (create-person #!key (seed #f))
  (if seed (randomize seed))
  (let* ((all-first-names (let loop ((n (+ 3 (- 2 (round (- (sqrt (sqrt (random 625))) 1)))))
                                     (result '()))
                            (cond
                             ((= 0 n) (reverse result))
                             (else (loop (- n 1) (cons (make-first-name) result))))))
         (full-name (string-intersperse all-first-names))
         (first-name (list-ref all-first-names (random (length all-first-names))))
         (middle-name (if (= 0 (random 40)) (make-last-name) #f))
         (last-name (make-last-name))
         (c/o (if (= 0 (random 40)) (make-c/o) #f))
         (street-number (if (< 1 (random 40)) (make-street-number) #f))
         (street-name-raw (make-street-name with-number-marker: #t))
         (street-name (string-replace-every "#" street-number street-name-raw))
         (postal-number (make-postal-number))
         (postal-address (make-postal-address))
         (phone-number (make-phone-number))
         (personal-number (make-social-security-number))
         (mobile-number (if (mobile-number? phone-number)
                            phone-number
                            (make-phone-number force-mobile: #t)))
         (email-address #f))
    (make-person
     full-name
     first-name
     middle-name
     last-name
     c/o
     street-name
     postal-number
     postal-address
     phone-number
     mobile-number
     email-address
     personal-number)))

(define (check-datum-uniqueness)
  (let loop ((rest datum)
             (counter 0)
             (result '()))
    (cond
     ((null? rest)
      (stderr counter " datums checked for duplicates.")
      result)
     (else
      (loop (cdr rest)
            (+ 1 counter)
            (if (find (lambda (datum)
                        (string=? (datum-word datum) (datum-word (car rest))))
                      (cdr rest))
                (cons (datum-word (car rest)) result)
                result))))))

(define (print-usage)
  (string-intersperse (list
                       "Usage: namnskada [flags] [n]"
                       "  -a    Make all data"
                       "  -c    Make record"
                       "  -d    Make postal address"
                       "  -e    Make phone number"
                       "  -f    Make all first names"
                       "  -g    Make registry entry"
                       "  -h    This text"
                       "  -l    Make last name"
                       "  -m    Make mobile phone number"
                       "  -n    Make full name [default]"
                       "  -o    Make spoken first name"
                       "  -p    Make place name"
                       "  -r    Make street name"
                       "  -s    Make social security number"
                       "  -u    Make postal number")
                      (conc #\newline)))

(and-let* ((args (command-line-arguments))
           (n (or (and (< 0 (length args))
                       (let ((n? (find string->number args)))
                         (and (string? n?)
                              (string->number n?))))
                  1)))
  (let* ((all (member "-a" args))
         (uniqueness (member "uniq" args))
         (record-style (member "-c" args))
         (address (member "-d" args))
         (street (member "-r" args))
         (place (member "-p" args))
         (registry (member "-g" args))
         (name (member "-n" args))
         (ss-number (member "-s" args))
         (po-number (member "-u" args))
         (phone-number (member "-e" args))
         (mobile-number (member "-m" args))
         (all-first-names (member "-f" args))
         (first-name (member "-o" args))
         (last-name (member "-l" args))
         (usage (member "-h" args))
         (proc (lambda ()
                 (let ((selected (filter identity
                                         (list
                                          (and uniqueness
                                               (check-datum-uniqueness))
                                          (and first-name
                                               (make-first-name))
                                          (and (or all-first-names all)
                                               (make-registry-style-name mark-proc: #f))
                                          (and (or last-name all)
                                               (make-last-name))
                                          (and (or ss-number all)
                                               (make-social-security-number))
                                          (and (or name all)
                                               (make-name))
                                          (and (or street all)
                                               (make-street-name))
                                          (and (or po-number all)
                                               (make-postal-number))
                                          (and (or place all)
                                               (make-place-name))
                                          (and (or phone-number all)
                                               (make-phone-number))
                                          (and (or mobile-number all)
                                               (make-phone-number force-mobile: #t))
                                          (and record-style
                                               (create-person))
                                          (and registry
                                               (make-address additional-data: #t))
                                          (and address
                                               (make-address))))))
                   (if (null? selected)
                       (list (make-name))
                       selected)))))
    (if usage
        (print (print-usage))
        (let loop ((i n))
          (cond
           ((= 0 i) n)
           (else
            (print (car (proc)))
            (loop (- i 1))))))))
