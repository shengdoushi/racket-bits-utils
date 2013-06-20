#lang racket

(provide byte->bit-list
         byte->hex-string
         hex-string->byte
         integer->hex-char
         hex-char->integer
         )

;; 字节转换为位表示
(define (byte->bit-list value)
  (define (iter p value count)
    (if (= count 0)
        p
        (iter (cons (remainder value 2) p) (quotient value 2) (- count 1))))
  (cond 
    ((byte? value)
     (iter '() value 8))
    ((char? value)
     (iter '() (char->integer value) 8))
    (else
     (error "please input a byte : " value))))

;; 字节的位表示转换为byte
(define (bit-list->byte bit-list)
  (define (iter product l)
    (if (null? l) 
        product
        (iter (+ (* 2 product) (car l)) (cdr l))))
  (iter 0 bit-list))

;; 字节转化为十六进制字符串表示
(define (byte->hex-string value)
  (define (sub-byte->char subbyte)
    (define (iter product bytes)
      (if (null? bytes)
          product
          (iter (+ (* product 2) (car bytes)) (cdr bytes))))
    (iter 0 subbyte))
  (let ((bit-list (byte->bit-list value)))
    (string 
     (integer->hex-char (sub-byte->char 
                     (list (first bit-list)
                           (second bit-list)
                           (third bit-list)
                           (fourth bit-list))))
     (integer->hex-char (sub-byte->char
                     (list-tail bit-list 4))))))

;; 字节的十六进制表示转换为字节值
(define (hex-string->byte hex-string)
  (let ((first-char 
         (if (> (string-length hex-string) 1)
             (string-ref hex-string 0)
             #\0))
        (second-char (string-ref hex-string (min 1 (- (string-length hex-string) 1)))))
    (+ (* 16 (hex-char->integer first-char))
       (hex-char->integer second-char))))
         
;;一个十六进制字符的值转化为对应的十六进制字符
(define (integer->hex-char hex-value)
  (cond ((and (>= hex-value 0)
              (<= hex-value 9))
         (integer->char (+ hex-value (char->integer #\0))))
        ((and (>= hex-value 10)
              (<= hex-value 15))
         (integer->char (+ (- hex-value 10) (char->integer #\A))))
        (else 
         (error "unknown integer->hex-char arg : " hex-value))))


;;一个十六进制字符转化为对应值
(define (hex-char->integer hex-char)
  (cond ((and (char>=? hex-char #\0)
              (char<=? hex-char #\9))
         (- (char->integer hex-char) 
            (char->integer #\0)))
        ((and (char>=? hex-char #\A)
              (char<=? hex-char #\F))
         (- (char->integer hex-char)
            (char->integer #\A)
            -10))
        ((and (char>=? hex-char #\a)
              (char<=? hex-char #\f))
         (- (char->integer hex-char)
            (char->integer #\a)
            -10))
        (else
         (error "unknown hex-char->integer arg: " hex-char))))
         
;; 字节的与操作（位运算) 
(define (byte-op fun byte1 byte2)
  (let ((result-list 
         (map fun
              (byte->bit-list byte1)
              (byte->bit-list byte2))))
    (bit-list->byte result-list)))

(define (byte-add byte1 byte2)
  (byte-op (lambda (a b)
             (if (and (= a 1) (= b 1)) 1 0))
           byte1
           byte2))

(define (byte-or byte1 byte2)
  (byte-op (lambda (a b)
             (if (or (= a 1) (= b 1)) 1 0))
           byte1
           byte2))

(define (byte-xor byte1 byte2)
  (byte-op (lambda (a b)
             (if (xor (= a 1) (= b 1)) 1 0))
           byte1
           byte2))