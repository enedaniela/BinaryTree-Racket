#lang racket

(require racket/include)
(include "binary-tree-test.rkt")

;; TASK pregatitor
(define binary-search-tree '(9 (3(2(1 () ()) ()) (6 (5 ()()) (8 (7 ()()) ()))) (12 (11 ()())(15 (13 ()())(21 ()())))))

(define empty-tree
  '())

(define init-node 
  (λ (value) 
    (make-node '() '() value))
  )

(define make-node
  (λ (left right value) 
    (list value left right))
  )

(define is-leaf?
  (λ (node) 
    (if (and (null? (get-left node)) (null? (get-right node)))
    #t
    #f))
  )

(define get-value
  (λ (node) 
    (if (null? node)
    '()
    (car node)))
  )

(define get-left
  (λ (node)
    (if (null? node)
    '()
     (cadr node)
  )))

(define get-right
  (λ (node)
   (if (null? node)
    '()
    (caddr node))
  ))

(define is-node?
  (λ (node) 
   (if (null? node)
    #f
    #t))
  )

(define is-empty?
  (λ (tree) 
    (null? tree))
  )

(define has-left?
  (λ (tree) 
    (if (equal? (get-left tree) '())
        #f
        #t
  ))
  )

(define has-right?
  (λ (tree) 
    (if (equal? (get-right tree) '())
        #f
        #t
  ))
  )

(define minimum
  (λ (tree) 
        (if (is-leaf? tree)
        (get-value tree)
        (minimum (get-left tree))
  ))
  )

(define maximum
  (λ (tree) 
     (if (is-leaf? tree)
        (get-value tree)
        (maximum (get-right tree))))
  )

(define height
  (λ (tree) 
    (if (is-empty? tree)
        0
    (+ 1 (max (height (get-left tree)) (height (get-right tree)))))
  ))


(define inorder-helper
      (lambda (tree L)
        (if (is-empty? tree) ;daca am parcurs tot arborele, returnez acumulatorul
            L
            (inorder-helper (get-left tree) ;parcurg stanga
                          (cons (car tree) ;apendez elementul
                                (inorder-helper (get-right tree) ;parcurg dreapta
                                              L))))))
(define inorder
  (λ (tree)    
    (inorder-helper tree '()))
  )

(define preorder
  (λ (tree) #f)
  )

(define postorder
  (λ (tree) #f)
  )

;functie care cauta un element in arbore
(define (search-tree e tree)
    (cond
        ((null? tree) #f) 
        ((= (get-value tree) e) #t) 
        ((> e (get-value tree)) (search-tree e (get-right tree)))
        ((< e (get-value tree)) (search-tree e (get-left tree)))
        
        
    ))

;incrementez valoarea data si caut de fiecare data elementul in arbore
;primul element gasit este succesorul
(define successor
  (λ (tree value)
    (if (search-tree ( + 1 value) tree)
        ( + 1 value)
        (successor tree ( + 1 value))
        )))

;decrementez valoarea data si caut de fiecare data elementul in arbore
;primul element gasit este predecesorul
(define predecessor
  (λ (tree value)
    (if (search-tree ( - value 1) tree)
        ( - value 1)
        (predecessor tree ( - value 1))
        )))

;;Task 1
(define (insert-helper item tree)
   (cond ((null? tree) (init-node item))
         ((= (get-value tree) item) tree)
         ((< item (get-value tree)) (balance (make-node 
                                             (insert-helper item (get-left tree))
                                             (get-right tree)
                                             (get-value tree))
                                             )
                                    )
         (else (balance (make-node 
                        (get-left tree)
                        (insert-helper item (get-right tree))
                        (get-value tree))
                        )
               )
         )
  )

(define insert
  (λ (tree value) (insert-helper value tree)
  ))

;functie ce intoarce parinele unui nod
;cu subarborii asociati
(define (parent-helper val parent-of-tree tree)
  (cond 
  ((eq? val (car tree)) parent-of-tree)
  ((< val (car tree)) (parent-helper val tree (get-left tree)))
  ((> val (car tree)) (parent-helper val tree (get-right tree)))))

(define (parent val tree)
  (parent-helper val empty-tree tree ))

;rotire la stanga
(define rotateLeftLeft
  (λ (tree)
    ;copilul stang al copilului stang ramane copilul stang
    (make-node (get-left (get-left tree))
    ;radacina devine copilul drept
               (make-node (get-right (get-left tree)) (get-right tree) (get-value tree)) 
    ;copilul stang devine noua radacina            
               (get-value (get-left tree)))
    )
  )

;rotire la dreapta
(define rotateRightRight
  (λ (tree)
    ;radacina devine copilul stang
    (make-node (make-node (get-left tree) (get-left (get-right tree)) (get-value tree))
    ;copilul drept al copilului drept ramane copilul drept           
               (get-right (get-right tree)) 
    ;copilul drept devine noua radacina
               (get-value (get-right tree)))
    )
  )

(define balance
  (λ (tree) 
     ;daca diferenta dintre subarborii stang si drept este 1, -1 sau 0,
     ;arborele este echilibrat
     (if (or ( = ( - (height (get-left tree)) (height (get-right tree))) 1) 
             ( = ( - (height (get-left tree)) (height (get-right tree))) -1)
             ( = ( - (height (get-left tree)) (height (get-right tree))) 0))
       tree
       ;daca diferenta este mai mare decat 1, se roteste corespunzator
       (if ( >= ( - (height (get-left tree)) (height (get-right tree))) 2)
           (rotateLeftLeft tree)
           (rotateRightRight tree)
           ))
    )
  )

(define union
  (λ (tree1 tree2) 
    (if (null? tree2)
        tree1
        (union 
         (union 
          ;pentru ca insert nu insereaza duplicate, inserez in tree1 valorile din tree2
          (insert tree1 (get-value tree2))
          ;apel recursiv pentru stanga
          (get-left tree2))
         ;apel recursiv pentru dreapta
        (get-right tree2)))
    )
  )
  
(define intersection
  (λ (tree1 tree2)
    (if (or (is-empty? tree2) (is-empty? tree1))
        '()
        ;daca valoarea din tree2 se regaseste in tree1
        (if (search-tree (get-value tree2) tree1)
            ;inserez valoarea in tree1 si apelez recursiv pentru tree2 
            ;format prin unirea partii stangi cu partea dreapta
            ;si excluderea valorii deja cautate
            (insert (intersection tree1 (union (get-left tree2) (get-right tree2))) (get-value tree2))
            ;daca valoarea nu este comuna, apelez fara insert
            (intersection tree1 (union (get-left tree2) (get-right tree2)))
            ))
    )
  )
            
(define complements
    (λ (tree1 tree2)
      ;apelez cu intersesctia pentru a obtine elementele care sunt in tree1, 
      ;dar nu si in intersectie
      (complements-helper (intersection tree1 tree2) tree1)
      )
  )

(define complements-helper
  (λ (tree1 tree2)
    (if (or (is-empty? tree2) (is-empty? tree1))
        '()
        (if (not (search-tree (get-value tree2) tree1))
            ;acelasi rationament ca la insert, doar ca inserez daca valoarea nu se gaseste in arbore
            (insert (complements-helper tree1 (union (get-left tree2) (get-right tree2))) (get-value tree2))
            (complements-helper tree1 (union (get-left tree2) (get-right tree2)))
            ))
    )
  )
;apel functie folosita la taskul 0
(define contains
  (λ (tree value) 
    (search-tree value tree)
    )
  )

(define remove
  (λ (tree value) 
    (if (null? tree)
        '()
        ;daca am ajuns la nodul pe care vreau sa il sterg/ iesire din recursivitate
        (cond  ((= value (get-value tree)) (if (is-leaf? tree) 
                                               '(); daca e frunza, o inlocuiesc cu null      
                                               (if (and (null? (get-right tree)) (not (null? (get-left tree)))) 
                                                   (get-left tree); daca este radacina unui arbore
                                                   ;cu legatura doar spre stanga, intorc subarborele
                                                   (if (and (null? (get-left tree)) (not (null? (get-right tree)))) 
                                                       (get-right tree); idem cu cazul de mai sus                                  
                                                       (union (get-right tree) (get-left tree)))))); daca am ambele ramuri, 
               ;elimin radacina, refac arborele
               ((> value (get-value tree));nu am ajuns inca la valoare, parcurg aborele pe dreapta 
                (balance (make-node  (get-left tree) (remove (get-right tree) value) (get-value tree))))
               ((< value (get-value tree)) ; parcurg arborele pe stanga
                (balance (make-node (remove (get-left tree) value) (get-right tree) (get-value tree)))))
        )  
    )
  )

;;Task 2
(define k-subsets
  (λ (set k) (subsets-helper (inorder set) k)
    )
  )

(define (subsets-helper L k)
    (if (eq? k 0);iesire din recursivitate
        (list '())
        (if (not(null? L))            
        (append (map (λ (x) (cons (car L) x)) (subsets-helper (cdr L) (- k 1)));construiesc combinarile fixand
                ;primul element cu toate combinarile de celelalte elemente
                (subsets-helper (cdr L) k)); aplic aceeasi logica, dar acum fixez urmatorul element
        null)
        )
  )
; permutari cu backtracking
(define (perms2 L)
  (let rec ((SolPart '()))
    (if (= (length SolPart) (length L))
        (list SolPart)
        (apply append (map
                       (λ (e)
                         (if (member e SolPart)
                             '()
                             (rec (cons e SolPart))
                             )) L)))))

(define (is-zigzag? L)
  (if (null? (cddr L))
      #t
      (if(or (and (< (car L) (cadr L))
               ( > (cadr L) (caddr L)))
          (and (> (car L) (cadr L))
               ( < (cadr L) (caddr L))))                    
          (is-zigzag? (cdr L))
          #f)))

(define (zig-zag-subsets L)
  (filter (λ (perm) (is-zigzag? perm)) (perms2 (inorder L))))


;;BONUS

(define (parser expr)
  (parse-helper expr '() '()))

(define (parse-helper expr operators operands)
  (cond ((null? expr)
	 (if (null? operators)
	     (car operands)
	     (make-op-node '() operators operands)))
	((number? (car expr));daca e numar, adica operator, creez frunza
         ;apelez recursiv pentru restul expresiei
	 (parse-helper (cdr expr)
		       operators
		       (cons (init-node (car expr)) operands)))
	((list? (car expr)); daca e o expresie, apelez recursiv pentru a parsa si aceasta expresie
	 (parse-helper (cdr expr)
		       operators
		       (cons (parser (car expr)) operands)))
        
	(else (if (or (null? operators)
		      (> (order (car expr))
			 (order (car operators))))
		  (parse-helper (cdr expr)
				(cons (car expr) operators)
				operands)
		  (make-op-node expr operators operands)))
        )
  )

(define (make-op-node expr operators operands);scot din lista de operatori si operanzi si creez subarbori
  (parse-helper expr
		(cdr operators)
		(cons (make-node (cadr operands) (car operands) (car operators))
		      (cddr operands))
                )
  )

;functie ce stabileste ordinea in functie de prioritatea semnelor
(define (order oper)
  (if (or (equal? oper '+)
          (equal? oper '-))
      1
      2))

(define (evaluate tree)
  ;daca expresia e un numar; intorc rezultatul
  (if (number? (car tree))
      (car tree)
  ;daca nu, folosesc rezultatul intors de find-operator ca si functie 
      ((find-operator (car tree))
         (evaluate (get-left tree))
         (evaluate (get-right tree)))))

(define (find-operator oper)
  (cond ((equal? oper '+) +)
	((equal? oper '-) -)
	((equal? oper '*) *)
	((equal? oper '/) /)
        )
  )

;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 30 puncte) ;;check-exp
(define functions (list is-node? is-leaf? is-empty? get-value make-node get-right get-left inorder height insert)) ;;check-exp
(define tree0 binary-search-tree) ;;check-exp
(check-exp-part 'is-node .037 (is-node? tree0) #t)
(check-exp-part 'is-leaf?1 .037 (is-leaf? tree0) #f)
(check-exp-part 'is-leaf?2 .037 (is-leaf? (init-node 8)) #t)
(check-exp-part 'is-empty?1 .037 (is-empty? tree0) #f)
(check-exp-part 'is-empty?2 .037 (is-empty? empty-tree) #t)
(check-exp-part 'get-value1 .037 (get-value tree0) 9)
(check-exp-part 'get-value2 .037 (get-value (get-left tree0)) 3)
(check-exp-part 'get-value3 .037 (get-value (get-right tree0)) 12)
(check-exp-part 'make-node .037 (make-node (get-left tree0) (get-right tree0) (get-value tree0)) binary-search-tree)
(check-exp-part 'minimum .0833 (minimum tree0) 1)
(check-exp-part 'maximum .0833 (maximum tree0) 21)
(check-exp-part 'height1 .0833 (height tree0) 5)
(check-exp-part 'height2 .0833 (height (get-left (get-left tree0))) 2)
(check-exp-part 'successor1 .055 (successor tree0 9) 11)
(check-exp-part 'successor2 .055 (successor tree0 5) 6)
(check-exp-part 'successor3 .055 (successor tree0 8) 9)
(check-exp-part 'predecessor1 .056 (predecessor tree0 9) 8)
(check-exp-part 'predecessor2 .056 (predecessor tree0 5) 3)
(check-exp-part 'predecessor3 .057 (predecessor tree0 12) 11)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(define A (create-tree '(8 9 10 15 8 5 0 1 4 5 9 7 1 0 151 651 61 45 416 2542 -8 3541 644 2 4 8542 51 142 215) functions)) ;;check-exp
(define B (create-tree '(942 4 54 64 94 25 0 -815 485 251 64 8 10 5 4 644 2 216 2541 5 8 7 5254 2542 214 4511) functions)) ;;check-exp
(define C (create-tree '(8 5 4 1 846 54 0 -5552 4 5 810 42 545 842 54 5488 8755 14 679 25 78 25 955 7891 789 8891 97 54 15 2465 155) functions)) ;;check-exp
(define D (create-tree '(8 9 1 5 9 7 5 9 78 1 5 6 9 89 24 52 95 22 94 6 485 18 6 97 8 100 4 9 655 478 92) functions)) ;;check-exp
(check-exp-part 'check-set1 .04 (test-task1 (create-tree '(8 4 2 1 -5 6 1 8 9 5 3 11 17 10 -6 4 8) functions) functions) result-check-set1)
(check-exp-part 'check-set2 .04 (test-task1 (create-tree '(-9 8 2 1 4 0 9 3 4 2 5 9 11 481 51 35 15 0 4 15 251 6551 12 3 4 7 9) functions) functions) result-check-set2)
(check-exp-part 'check-set3 .04 (test-task1 A functions) result-check-set3)
(check-exp-part 'check-set4 .04 (test-task1 B functions) result-check-set4)
(check-exp-part 'check-set5 .04 (test-task1 C functions) result-check-set5)
(check-exp-part 'union1 .005 (test-task1 (union A B) functions) result-union1)
(check-exp-part 'union2 .005 (test-task1 (union C D) functions) result-union2)
(check-exp-part 'union3 .005 (test-task1 (union A D) functions) result-union3)
(check-exp-part 'union4 .005 (test-task1 (union (union A B) (union C D)) functions) result-union4)
(check-exp-part 'intersection1 .01 (test-task1 (intersection A B) functions) result-intersection1)
(check-exp-part 'intersection2 .01 (test-task1 (intersection B C) functions) result-intersection2)
(check-exp-part 'intersection3 .01 (test-task1 (intersection C D) functions) result-intersection3)
(check-exp-part 'intersection4 .01 (test-task1 (intersection (intersection A B) (intersection  C D)) functions) result-intersection4)
(check-exp-part 'complements1 .01 (test-task1 (complements A B) functions) result-complements1)
(check-exp-part 'complements2 .01 (test-task1 (complements C D) functions) result-complements2)
(check-exp-part 'complements3 .01 (test-task1 (complements C D) functions) result-complements3)
(check-exp-part 'complements4 .01 (test-task1 (complements (complements A B) (complements C D)) functions) result-complements4)
(check-exp-part 'insert1 .005 (test-task1 (insert B -7) functions) result-insert1)
(check-exp-part 'insert2 .005 (test-task1 (insert A 59525) functions) result-insert2)
(check-exp-part 'insert3 .005 (test-task1 (insert C 988522) functions) result-insert3)
(check-exp-part 'insert4 .005 (test-task1 (insert D -812612) functions) result-insert4)
(check-exp-part 'remove1 .02 (test-task1 (remove binary-search-tree (minimum binary-search-tree)) functions) result-remove1)
(check-exp-part 'remove2 .02 (test-task1 (remove binary-search-tree 9) functions) result-remove2)
(check-exp-part 'remove3 .02 (test-task1 (remove binary-search-tree 3) functions) result-remove3)
(check-exp-part 'remove4 .02 (test-task1 (remove (remove (remove A (successor A 10)) (predecessor A 0)) 416) functions) result-remove4)
(check-exp-part 'complex1 .02 (test-task1 (union A (intersection B C)) functions) result-complex1)
(check-exp-part 'complex2 .02 (test-task1 (insert (intersection (complements B C) (remove (union A B) (predecessor A 51))) 7851) functions) result-complex2)
(check-exp-part 'complex3 .02 (test-task1 (insert (remove (remove (union (intersection (complements B C) (complements B A)) binary-search-tree) 214) 1) 1) functions) result-complex3)
(check-exp-part 'complex4 .02 (test-task1 (union (intersection (complements B A) (union C D)) (complements A D)) functions) result-complex4)
(check-exp-part 'complex5 .02 (test-task1 (intersection (union (complements A B) (complements C D)) (complements (intersection A B) (intersection C D))) functions) result-complex5)
(check-exp-part 'complex6 .02 (test-task1 (remove (insert (union (union (complements A B) (intersection C D)) (intersection (complements C D) (intersection A B))) 22) -8) functions) result-complex6)
(check-exp-part 'complex7 .02 (test-task1 (union (union (intersection A C) (complements A D)) (intersection (complements B C) (intersection B D))) functions) result-complex7)
(check-exp-part 'complex8 .02 (test-task1 (union (union (union A B) (union C D)) (intersection (intersection A B) (intersection C D))) functions) result-complex8)
(check-exp-part 'complex9 .02 (test-task1 (intersection (union (complements A B) (complements B A)) (intersection (union A B) (union C D))) functions) result-complex9)
(check-exp-part 'complex10 .02 (test-task1 (insert (remove (intersection (union (complements B A) (union (complements C D) (intersection A B))) (intersection (complements B (union A C)) (union C D))) 485) 100) functions) result-complex10)
(check-exp-part 'height-balanced1 .04 (check-self-balancing-tree B functions) #t)
(check-exp-part 'height-balanced2 .04 (check-self-balancing-tree A functions) #t)
(check-exp-part 'height-balanced3 .04 (check-self-balancing-tree C functions) #t)
(check-exp-part 'height-balanced4 .04 (check-self-balancing-tree D functions) #t)
(check-exp-part 'height-balanced5 .04 (let [(tree (create-tree '(1 2 3 4 5 6 7 8 9 10) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced6 .04 (let [(tree (create-tree '(20 19 18 17 16 15 10 9 8 7 6 5 4 3 2 1) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced7 .04 (let [(tree (union A (intersection B C)))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced8 .04 (let [(tree (remove (insert (union (complements A D) (intersection B C)) 24) 416))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced9 .04 (let [(tree (union (remove binary-search-tree 9) A))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced10 .04 (let [(tree (intersection (union (remove A (get-value A)) (remove B (get-value B))) (remove C (get-value C))))] (check-self-balancing-tree tree functions)) #t)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - TASK 2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 2 : 20 puncte) ;;check-exp
(check-exp-part 'k-subsets1 0.1 (k-subsets (intersection A B) 8) result-k-subsets1)
(check-exp-part 'k-subsets2 0.1 (let [(subsets (k-subsets binary-search-tree 11))] (and (= (length subsets) 78) (not (equal? (member '(2 3 5 6 8 9 11 12 13 15 21) subsets) #f)))) #t)
(check-exp-part 'k-subsets3 0.1 (k-subsets (create-tree '(1 2 3 4 5) functions) 3) result-k-subsets3)
(check-exp-part 'k-subsets4 0.1 (k-subsets (create-tree '(8 7 6 5) functions) 2) result-k-subsets4)
(check-exp-part 'k-subsets5 0.1 (k-subsets D 20) result-k-subsets5)
(check-exp-part 'zig-zag-subsets1 0.1 (zig-zag-subsets (create-tree '(1 2 3 4 5 6) functions)) result-zig-zag1)
(check-exp-part 'zig-zag-subsets2 0.1 (zig-zag-subsets (create-tree '(1 2 3 4) functions)) result-zig-zag2)
(check-exp-part 'zig-zag-subsets3 0.1 (zig-zag-subsets (create-tree '(1 7 9 10 5) functions)) result-zig-zag3)
(check-exp-part 'zig-zag-subsets4 0.1 (zig-zag-subsets (create-tree '(98 5 1 -85 -457) functions)) result-zig-zag4)
(check-exp-part 'zig-zag-subsets5 0.1 (length (zig-zag-subsets (create-tree '(982 616 542 125 98 85) functions))) 122)
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 0.1 (test-bonus (parser '(1 + (((2 * 3) - 4) * 5))) functions) 11)
(check-exp-part 'bonus2 0.1 (test-bonus (parser '((((5 + 8) * (9 - (8 / 2))) + (8 * 9)) * 10)) functions) 1370)
(check-exp-part 'bonus3 0.1 (test-bonus (parser '((5 * 8) - (7 * (3 + (5 * (10 / 2)))))) functions) -156)
(check-exp-part 'bonus4 0.1 (test-bonus (parser '(((((80 - 78) + 15) * 4 ) / 2) + (7 + (((5 * 3) - 2) * 4)))) functions) 93)
(check-exp-part 'bonus5 0.2 (test-bonus (parser '(((((((((5 + 8) + (9 + 8)) * 3) + (8 - 7)) * 2) + 10) / 2) * 10) - (5 + (7 + (8 * (1 + 2)))))) functions) 924)
(check-exp-part 'bonus6 0.2 (test-bonus (parser '((((((5 + 6) * 7) + 9) * 10) / 2) + (7 * (2 * (4 * (10 - (7 + (1 * (2 - 1))))))))) functions) 542)
(check-exp-part 'bonus7 0.2 (test-bonus (parser '(((5 + (7 - (2 * (3 + (9 - (7 + (8 + (5 * 2)))))))) + (5 * (((2 + 2) * (3 + 7)) + (7 * (9 - (4 + 7)))))) / 2)) functions) 84)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE

(sumar)