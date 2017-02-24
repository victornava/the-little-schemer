#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x))(not (null? x)))))

(define lat?
  (lambda (l)
   (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

 (lat? `(uno dos tres))

;============================================================================

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(member? `meat `())
(member? `meat `(mashed potatoes and meat gravy))

; ============================================================================

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))

(member? `meat `())
(rember `sauce `(soy sauce and tomato sauce))

; ============================================================================

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
              (firsts (cdr l)))))))

; ============================================================================

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
              (cons old
                (cons new (cdr lat))))
              (else (cons (car lat)
                      (insertR new old
                        (cdr lat)))))))))

(insertR `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new lat))
              (else (cons (car lat)
                      (insertL new old
                        (cdr lat)))))))))

(insertL `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat)
                      (subst new old
                        (cdr lat)))))))))

(subst `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
               ((or (eq? (car lat) o1) (eq? (car lat) o2))
                (cons new (cdr lat)))
               (else (cons (car lat)
                      (subst new o1 o2
                        (cdr lat)))))))))

(subst2 `vanilla `chocolate `banana `(banana ice cream with chocolate topping))

; ============================================================================

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a)
            (multirember a (cdr lat)))
          (else
            (cons (car lat)
              (multirember a (cdr lat)))))))))

(multirember `cup `(coffee cup tea cup and hick cup))

; ============================================================================

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
                (cons old
                  (cons new
                    (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                      (multiinsertR new old
                        (cdr lat)))))))))

(multiinsertR `fried `fish `(chips and fish or fish and fried))

; ============================================================================

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
                (cons new
                  (cons old
                    (multiinsertL new old (cdr lat)))))
              (else (cons (car lat)
                      (multiinsertL new old (cdr lat)))))))))

(multiinsertL `fried `fish `(chips and fish or fish and fried))

; ============================================================================

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) old)
             (cons new
               (multisubst new old (cdr lat))))
          (else
            (cons (car lat)
              (multisubst new old (cdr lat)))))))))

(multisubst `pedro `sam `(sam is my name señor sam))

; ============================================================================

(define add1
  (lambda (n)
    (+ n 1)))

(add1 2)

; ============================================================================

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 2)

; ============================================================================

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(plus 46 12)

; ============================================================================

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))))))

(minus 46 12)

; ============================================================================

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(addtup `(1 2 3))

; ============================================================================

(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (multiply n (sub1 m)))))))

(multiply 12 3)

; ============================================================================

(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else
        (cons (+ (car t1) (car t2))
              (tup+ (cdr t1) (cdr t2)))))))

(tup+ `(3 7) `(4 6))
(tup+ `(3 7) `(4 6 8 1))

; ============================================================================

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 2 1)
(> 1 2)
(> 2 2)

; ============================================================================

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(< 2 1)
(< 1 2)
(< 2 2)

; ============================================================================

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(= 2 1)
(= 1 2)
(= 2 2)

; ============================================================================

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (pow n (sub1 m)))))))


(pow 1 1)
(pow 2 3)
(pow 5 3)

; ============================================================================

(define divide
  (lambda (n m)
    (cond
      ((< n m) 0 )
      (else (add1 (divide (minus n m) m))))))

(divide 15 4)

; ============================================================================

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
        (add1 (length (cdr lat)))))))

(length `(hotdogs with mustard sauerkraut and pickles))
(length `(ham and cheese on rye))

; ============================================================================

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat) )))))

(pick 4 `(lasagna spaghetti ravioli macaroni meatball))

; ============================================================================

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
          (rempick (sub1 n)
            (cdr lat)))))))


(rempick 4 `(hotdogs with hot mustard))

; ============================================================================

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      (else
        (cond
          ((number? (car lat))
            (no-nums (cdr lat)))
          (else
            (cons (car lat)
              (no-nums (cdr lat)))))))))

(no-nums `(5 pears 6 prunes 9 dates)) ; -> (pears prunes dates)

; ============================================================================

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      (else
        (cond
          ((number? (car lat))
            (cons (car lat) (all-nums (cdr lat))))
          (else
            (all-nums (cdr lat))))))))

(all-nums `(5 pears 6 prunes 9 dates)) ; -> (5 6 9)


; ============================================================================


(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
        (= a1 a2))
      ((or (number? a1) (number? a2))
        #f)
      (else (eq? a1 a2)))))

(eqan? 1 1) ; -> #t
(eqan? 2 1) ; -> #f
(eqan? `a `a) ; -> #t
(eqan? `a `b) ; -> #f

; ============================================================================

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
        (cond
          ((eq? (car lat) a)
            (add1 (occur a (cdr lat))))
          (else
            (occur a (cdr lat))))))))

(occur 3 `(1 2 3 4 5 3)) ; 2

; ============================================================================

(define one?
  (lambda (n)
    (= n 1)))

(one? 1) ; #t
(one? 2) ; #f

; ============================================================================

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
          (rempick2 (sub1 n)
            (cdr lat)))))))


(rempick2 4 `(hotdogs with hot mustard))

; ============================================================================

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) a)
            (rember* a (cdr l)))
          (else
            (cons (car l)
              (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l))
          (rember* a (cdr l)))))))


(rember* `cup `((coffee) cup ((tea) cup) (and (hick)) cup))

; ============================================================================

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons old (cons new (insertR* new old (cdr l)))))
          (else
            (cons (car l) (insertR* new old (cdr l))))))
      (else
        (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))


(insertR* `roast `chuck `(chuck other (chuck)))
(insertR* `roast `chuck `(chuck other chuck))
(insertR* `roast `chuck `((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))

; ============================================================================

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (subst* new old (cdr l))))
          (else
            (cons (car l) (subst* new old (cdr l))))))
      (else
        (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* `topping `fudge `(ice cream with fudge for dessert))
(subst* `topping `fudge `(ice cream with fudge for dessert fudge))
(subst* `orange `banana `((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))

; ============================================================================

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cons old (insertL* new old (cdr l)))))
          (else
            (cons (car l) (insertL* new old (cdr l))))))
      (else
        (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* `pecker `chuck `((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))

; ============================================================================

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or
          (eq? (car l) a)
          (member* a (cdr l))))
      (else
        (or
          (member* a (car l))
          (member* a (cdr l)))))))

(member* `chips `(potato chips fish))
(member* `chips `(potato (chips) with fish))
(member* `chips `((potato) (chips ((with) fish) (chips))))

; ============================================================================

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
        (leftmost (car l))))))

(leftmost `(((hot) (tuna (and))) cheese))
(leftmost `((potato) (chips ((with) fish) (chips))))

; ============================================================================

(define eqlist?
 (lambda (l1 l2)
   (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1))
       (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1))
       (atom? (car l2)))
      #f)
     (else
       (and (eqlist? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2)))))))

(eqlist?
 `(beef ((sausage) (and (soda))))
 `(beef ((sausage) (and (soda)))))

; ============================================================================

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and
          (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp)))))))))

(numbered? 1)
(numbered? `(3 + ( 4 x 5)))
(numbered? `(2 x sausage))

; ============================================================================

(define 1st-sub-exp
  (lambda (axep)
    (car (cdr axep))))

(1st-sub-exp `(+ 1 2))

; ============================================================================

(define 2st-sub-exp
  (lambda (axep)
    (car (cdr (cdr axep)))))

(2st-sub-exp `(+ 1 2))

; ============================================================================

(define operator
  (lambda (axep)
    (car axep)))

(operator `(+ 1 2))

; ============================================================================

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) `+)
        (plus
          (value (1st-sub-exp nexp))
          (value (2st-sub-exp nexp))))
      ((eq? (operator nexp) `-)
        (minus
          (value (1st-sub-exp nexp))
          (value (2st-sub-exp nexp))))
      (else
        (multiply
          (value (1st-sub-exp nexp))
          (value (2st-sub-exp nexp)))))))

(value `(+ 5 2))
(value `(- 5 2))
(value `(x 5 2))
(value `(+ (x 3 6) (- 8 2)))
