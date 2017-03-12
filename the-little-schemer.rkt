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
      ((null? lat) `())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))

(member? `meat `())
(rember `sauce `(soy sauce and tomato sauce))

; ============================================================================

(define firsts
  (lambda (l)
    (cond
      ((null? l) `())
      (else (cons (car (car l))
              (firsts (cdr l)))))))

; ============================================================================

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? lat) `())
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
      ((null? l) `())
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
      ((null? l) `())
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
      ((null? l) `())
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
      ((null? l) `())
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
      ((eq? (operator nexp) `x)
        (multiply
          (value (1st-sub-exp nexp))
          (value (2st-sub-exp nexp))))
      (else
        (pow
          (value (1st-sub-exp nexp))
          (value (2st-sub-exp nexp)))))))

(value `(+ 1 2)) ; 3
(value `(x 3 6)) ; 18
(value `(^ 8 2)) ; 64
(value `(+ (x 3 6) (^ 8 2))) ;82

; ============================================================================

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else
        (set? (cdr lat))))))

(set? `())
(set? `(apple))
(set? `(apple peaches apple plum))
(set? `(apples peaches pears plums))

; ============================================================================

(define makeset
  (lambda (lat)
    (cond
      ((null? lat)`())
      (else
        (cons
          (car lat)
          (makeset
            (multirember (car lat) (cdr lat))))))))

(makeset `(apple peach pear peach plum apple lemon peach))
(makeset `(apple 3 pear 4 9 apple 3 4))

; ============================================================================

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (and
          (member? (car set1) set2)
          (subset? (cdr set1) set2))))))

(subset?
  `(5 chicken wings)
  `(5 hamburgers 2 pieces fried chicken and light duck wings))

(subset?
  `(5 chicken wings)
  `(2 pieces fried chicken and light duck wings))

; ============================================================================

(define eqset?
  (lambda (set1 set2)
    (and
      (subset? set1 set2))
      (subset? set2 set1)))

(eqset?
  `(6 large chickens with wings)
  `(6 chickens with large wings))

; ============================================================================

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (or
          (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

(intersect?
  `(stewed tomatoes and macaroni)
  `(macaroni and cheese))

; ============================================================================

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) `())
      ((member? (car set1) set2)
        (cons
          (car set1)
          (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)))))

(intersect
  `(stewed tomatoes and macaroni)
  `(macaroni and cheese))

; ============================================================================

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
        (union (cdr set1) set2))
      (else
        (cons (car set1)
          (union (cdr set1) set2))))))

(union
  `(stewed tomatoes and macaroni caserole)
  `(macaroni and cheese))

; ============================================================================

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
        (intersect
          (car l-set)
          (intersectall (cdr l-set)))))))

(intersectall `((a b c) (c a de) (e f g h a b)))

; ============================================================================

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? `(pear pear))
(a-pair? `((2) (pair)))

; ============================================================================

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? `((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? `((d 4) (b 0) (b 9) (e 5) (g 4)))

; ============================================================================

(define first
  (lambda (x)
    (car x)))

(first `(uno dos))
(first `((uno) (dos)))

; ============================================================================

(define second
  (lambda (x)
    (car (cdr x))))

(second `(uno dos))
(second `((uno) (dos)))

; ============================================================================

(define build
  (lambda (s1 s2)
    (cond
      (else
        (cons s1 (cons s2 `()))))))

(build `uno `dos)

; ============================================================================

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(revpair `(uno dos))

; ============================================================================

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) `())
      (else
        (cons
          (revpair (car rel))
          (revrel (cdr rel)))))))

(revrel `((8 a) (pumpkin pie) (got sick)))

; ============================================================================

(define seconds
  (lambda (l)
    (cond
      ((null? l) `())
      (else (cons (second (car l))
              (seconds (cdr l)))))))

(seconds `((1 2) (3 4) (5 6)))

; ============================================================================

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))


(one-to-one? `((grape raisin)
               (plum prune)
               (stewed prune)))

(one-to-one? `((grape raisin)
               (plum prune)
               (stewed grape)))

; ============================================================================
; CHAPTER 8
; ============================================================================

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) `())
      ((test? (car l) a) (cdr l))
      (else
        (cons
          (car l)
          (rember-f test? a (cdr l)))))))

(rember-f = 5 `(6 2 5 3))
(rember-f eq? `jelly `(jelly beans are good))
(rember-f equal? `(pop corn) `(lemonade (pop corn) and (cake)))

; ============================================================================

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) `())
        ((test? (car l) a) (cdr l))
        (else
          (cons (car l)
            ((rember-f2 test?) a
             (cdr l))))))))

((rember-f2 eq?) `tuna `(shrimp salad and tuna salad))
((rember-f2 eq?) `eq? `(equal? eq? eqan? eqpair?))

; ============================================================================

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) `())
        ((test? (car l) old)
          (cons new (cons old (cdr l))))
        (else
          (cons (car l)
          ((insertL-f test?) new old (cdr l))))))))

((insertL-f eq?) `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) `())
        ((test? (car l) old)
          (cons old (cons new (cdr l))))
        (else
          (cons (car l)
          ((insertR-f test?) new old (cdr l))))))))

((insertR-f eq?) `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) `())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else
          (cons (car l)
          ((insert-g seq) new old (cdr l))))))))

((insert-g seqL) `topping `fudge `(ice cream with fudge for dessert))
((insert-g seqR) `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst3 (insert-g seqS))

(subst3 `topping `fudge `(ice cream with fudge for dessert))

; ============================================================================


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x `+) plus)
      ((eq? x `x) multiply)
      (else pow))))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
          (value2 (1st-sub-exp nexp))
          (value2 (2st-sub-exp nexp)))))))

(value2 `(+ 1 2)) ; 3
(value2 `(x 3 6)) ; 18
(value2 `(^ 8 2)) ; 64
(value2 `(+ (x 3 6) (^ 8 2))) ;82

; ============================================================================

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) `())
        ((test? a (car lat))
          ((multirember-f test?) a (cdr lat)))
          (else
            (cons (car lat)
              ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) `tuna `(shrimp salad tuna salad and tuna))

; ============================================================================

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) `())
      ((test? (car lat))
      (multiremberT test? (cdr lat)))
        (else
          (cons
            (car lat)
            (multiremberT test? (cdr lat)))))))

(define eq?-tuna
  (lambda (a)
    (eq? `tuna a)))

(multiremberT eq?-tuna `(shrimp salad tuna salad and tuna))

; ============================================================================

(define even?
  (lambda (n)
    (= (multiply (divide n 2) 2) n)))

(define evens-only* (lambda (l)
  (cond
    ((null? l) `())
    ((atom? (car l))
      (cond
        ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
        (else
          (evens-only* (cdr l)))))
      (else
        (cons
          (evens-only* (car l))
          (evens-only* (cdr l)))))))

(evens-only*  `((9 1 28) 3 10 ((9 9) 76) 2))

; ============================================================================
; CHAPTER 9
; ============================================================================

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
        (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;(looking `caviar `(6 2 4 caviar 5 7 3))
;(looking `caviar `(7 2 4 7 5 6 3))

; Processing the rest...

; ============================================================================
; CHAPTER 10
; ============================================================================

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
        (car values))
      (else
      (lookup-in-entry-help
        name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      entry-f)))

(define entry-not-found
  (lambda (entry)
  (cons entry `(not found))))


(lookup-in-entry
  `entree
  `((appetizer entree beverage) (food tastes good))
  entry-not-found)

(lookup-in-entry
  `dessert
  `((appetizer entree beverage) (food tastes good))
  entry-not-found)


(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
        (car table)
        (lambda (name)
          (lookup-in-table name
            (cdr table) table-f)))))))

(lookup-in-table
  `entrée
  `(((entrée dessert)
     (spaghetti spumoni))
    ((appetizer entrée beverage)
     (food tastes good)))
  entry-not-found)

; ============================================================================
; FIN
; ============================================================================