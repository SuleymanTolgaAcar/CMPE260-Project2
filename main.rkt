;suleyman tolga acar
;2021400237
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

; Converts a binary number given as a string of bits to a decimal number
(define (binary_to_decimal binary)
  (string->number binary 2))

; Adds the given base to the decimal representation of each binary number in the list if it is less than or equal to the limit
(define (relocator args limit base)
  (map (lambda (x) (if (<= (binary_to_decimal x) limit) (+ base (binary_to_decimal x)) -1)) args))

; Splits the given address space into page number and page offset and returns them as a list of two strings
(define (divide_address_space num page_size)
  (list (substring num 0 (- (string-length num) (+ (exact-floor (/ (log page_size) (log 2))) 10)))
        (substring num (- (string-length num) (+ (exact-floor (/ (log page_size) (log 2))) 10)) (string-length num))))

; Maps the page number part of the addresses to the corresponding page table entry and returns the list of the resulting addresses
(define (page args page_table  page_size)
  (map (lambda (x) (string-append (list-ref page_table (binary_to_decimal (car (divide_address_space x page_size)))) 
                                  (car (cdr (divide_address_space x page_size))))) args))

; Finds the sin value of the given value in degrees using the Taylor series expansion
(define (find_sin value num)
    (if (<= num 1)
        (* value (/ pi 180))
        (+ (* (/ (expt -1 (+ num 1)) (factorial (- (* 2 num) 1))) (expt (* value (/ pi 180)) (- (* 2 num) 1))) (find_sin value (- num 1)))))

(define (factorial num)
    (if (<= num 1)
        1
        (* num (factorial (- num 1)))))

; Hashes the given address using the sum of digits of the sin value of the address and returns the hash value
(define (myhash arg table_size)
    (remainder (sum_digits (exact-floor (* (find_sin (binary_to_decimal arg) (+ (remainder (binary_to_decimal arg) 5) 1)) (expt 10 10))) 10) table_size))

; Helper function to find the sum of the digits of the given value until the given number of digits
(define (sum_digits value num)
    (if (<= num 0)
        0
        (+ (remainder value 10) (sum_digits (quotient value 10) (- num 1)))))

; Hashes the page number part of the address using the myhash function, finds the corresponding page table entry and returns the hashed address
(define (hashed_page arg table_size page_table page_size) 
    (let ((divided_arg (divide_address_space arg page_size)))
        (string-append (find_tail (car divided_arg) (list-ref page_table (myhash (car divided_arg) table_size))) (car (cdr divided_arg)))))

; Helper function to find the value in the list of lists and return the tail of the inner list
(define (find_tail value lst)
    (if (equal? (caar lst) value)
        (cadar lst)
        (find_tail value (cdr lst))))

; Splits the given steam of addresses into chunks of the given size and returns them as a list of lists
(define (split_addresses args size)
    (if (equal? args "") '()
        (cons (list (substring args 0 size)) (split_addresses (substring args size) size))))

; Maps the addresses to the corresponding hashed addresses and returns the list of the resulting addresses
(define (map_addresses args table_size page_table page_size address_space_size)
    (map (lambda (x) (hashed_page (car x) table_size page_table page_size)) (split_addresses args address_space_size)))
