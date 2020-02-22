#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define (push element stack) (append (list element) stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC)) 

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (list-ref stack-machine 1))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (list-ref stack-machine 2))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (list-ref stack-machine 3))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (list-ref stack-machine 4))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (list-ref stack-machine 0))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (list-ref stack-machine 5))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (case symbol
    ['STACK 0]
    ['CO-VARNAMES 1]
    ['CO-CONSTS 2]
    ['CO-NAMES 3]
    ['CO-CODE 4]
    ['INSTRUCTION-COUNTER 5]))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (run-stack-machine-helper stack-machine 0))

;functii pentru implementarea iteratorilor
(define current car)
(define next cdr)

(define (run-stack-machine-helper stack-machine counter)
  (if (equal? counter (length (get-code stack-machine)))
      stack-machine
      (case (car (list-ref (get-code stack-machine) counter))
        ['POP_TOP (run-stack-machine-helper (pop-exec-stack stack-machine) (+ 1 counter))]

        ;retinem ca get-code intoarce o lista de perechi
        
        ['LOAD_CONST (run-stack-machine-helper (push-exec-stack (hash-ref (get-consts stack-machine) (cdr (list-ref (get-code stack-machine) counter))) stack-machine) (+ 1 counter))]

        ['LOAD_GLOBAL (run-stack-machine-helper (push-exec-stack (hash-ref (get-names stack-machine) (cdr (list-ref (get-code stack-machine) counter))) stack-machine) (+ 1 counter))]
        
        ['LOAD_FAST (run-stack-machine-helper (push-exec-stack (hash-ref (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) counter))) stack-machine) (+ 1 counter))]
        
        ['STORE_FAST (run-stack-machine-helper (make-stack-machine (pop (get-stack stack-machine)) (hash-set (get-varnames stack-machine)
             (cdr (list-ref (get-code stack-machine) counter)) (top (get-stack stack-machine))) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine)) (+ 1 counter))]
        
        ['RETURN_VALUE (run-stack-machine-helper stack-machine (+ 1 counter))]
        
        ['BINARY_ADD (run-stack-machine-helper (push-exec-stack (+ (top (get-stack stack-machine)) (top (get-stack (pop-exec-stack stack-machine)))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['BINARY_SUBTRACT (run-stack-machine-helper (push-exec-stack (- (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['BINARY_MODULO (run-stack-machine-helper (push-exec-stack (modulo (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['INPLACE_MODULO (run-stack-machine-helper (push-exec-stack (modulo (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['INPLACE_SUBTRACT (run-stack-machine-helper (push-exec-stack (- (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['INPLACE_ADD (run-stack-machine-helper (push-exec-stack (+ (top (get-stack stack-machine)) (top (get-stack (pop-exec-stack stack-machine)))) (make-stack-machine
             (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))]
        
        ['JUMP_ABSOLUTE (run-stack-machine-helper stack-machine (/ (cdr (list-ref (get-code stack-machine) counter)) 2))]
        
        ['POP_JUMP_IF_FALSE (if (false? (top (get-stack stack-machine)))
                                (run-stack-machine-helper (pop-exec-stack stack-machine) (/ (cdr (list-ref (get-code stack-machine) counter)) 2))
                                (run-stack-machine-helper (pop-exec-stack stack-machine) (+ 1 counter)))]
        
        ['POP_JUMP_IF_TRUE (if (false? (top (get-stack stack-machine)))
                                (run-stack-machine-helper (pop-exec-stack stack-machine) (+ 1 counter))
                                (run-stack-machine-helper (pop-exec-stack stack-machine) (/ (cdr (list-ref (get-code stack-machine) counter)) 2)))]
        ['COMPARE_OP (run-stack-machine-helper (push-exec-stack ((get-cmpop (cdr (list-ref (get-code stack-machine) counter))) (top (get-stack (pop-exec-stack stack-machine)))
                        (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack stack-machine))) (+ 1 counter))]
        ['POP_BLOCK (run-stack-machine-helper stack-machine (+ 1 counter))]

        ['SETUP_LOOP (run-stack-machine-helper stack-machine (+ 1 counter))]

        ['GET_ITER (run-stack-machine-helper stack-machine (+ 1 counter))]

        ['FOR_ITER (if (null? (top (get-stack stack-machine)))
                       (run-stack-machine-helper (pop-exec-stack stack-machine) (+ (+ 1 counter) (/ (cdr (list-ref (get-code stack-machine) counter)) 2)))
                       (run-stack-machine-helper (push-exec-stack (car (top (get-stack stack-machine))) (update-stack-machine
                        (list-set (get-stack stack-machine) 0 (next (top (get-stack stack-machine)))) 'STACK stack-machine)) (+ 1 counter)))]
        
        ['CALL_FUNCTION (run-stack-machine-helper (push-exec-stack (apply (get-function (list-ref (get-stack stack-machine) (cdr (list-ref (get-code stack-machine) counter)))) (take (get-stack stack-machine) (cdr (list-ref (get-code stack-machine) counter))))
                           (make-stack-machine (list-tail (get-stack stack-machine) (add1 (cdr (list-ref (get-code stack-machine) counter)))) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                           (get-code stack-machine) (get-IC stack-machine))) (+ 1 counter))] 
        )))
