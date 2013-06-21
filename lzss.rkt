#lang racket
(require "bits.rkt")

(provide lzss-decoder
         make-lzss-default-decoder
         )

(define (make-lzss-default-decoder)
  (lzss-decoder 4096 18 2))

(define (lzss-decoder N F THRESHOLD)
  (lambda (port outport)
    (let ((cread 0)
          (i 0)
          (j 0)
          (r (- N F))
          (text-buf (make-bytes (+ N F -1) (char->integer #\ )))
          (flags 0))
      
      ;; init
          
      ;; safe read
      (define (read-c)
        (set! cread (read-byte port))
        (not (eof-object? cread)))
      (define (read-i)
        (set! i (read-byte port))
        (not (eof-object? i)))
      (define (read-j)
        (set! j (read-byte port))
        (not (eof-object? j)))
      
      ;; 读入第一个字节
      (define (pre-read-char)
        (set! flags (integer>> flags 1))
        (cond ((= 0 (integer-add flags 256))
               (and (read-c)
                    (begin
                      (set! flags (integer-or cread (hex-string->integer "FF00")))
                      #t)))
              (else
               #t)))
      
      
      ;;读入下一个字节内容(原始的)
      (define (read-content-ori)
        (and 
         (read-c)
         (begin
           (write-byte cread outport)
           (bytes-set! text-buf r cread)
           (set! r (integer-add (+ r 1) (- N 1))))))
      
      ;; 读入下面的压缩过的内容
      (define (read-content-pack)
        (define (iter k)
          (when (<= k j)
            (set! cread (bytes-ref text-buf (integer-add (+ i k) (- N 1))))
            (write-byte cread outport)
            (bytes-set! text-buf r cread)
            (set! r (integer-add (+ r 1) (- N 1)))
            (iter (+ k 1))))
        (and (read-i)
             (read-j)
             (begin
               (set! i (integer-or i (integer<< (integer-add j (hex-string->byte "F0")) 4)))
               (set! j (+ (integer-add j (hex-string->byte "0F")) THRESHOLD))
               (iter 0)
               )))
      
      ;; 主循环
      (define (loop-iter)
        (let ((ret
               (and (pre-read-char)
                    (if (not (= 0 (integer-add flags 1)))
                        (read-content-ori)
                        (read-content-pack)))))
          (when ret
            (loop-iter))))
      (loop-iter)
    )))
