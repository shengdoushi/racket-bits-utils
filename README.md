
use to debug bytes


(byte->bit-list 3)

=> (0 0 0 0 0 0 1 1)



(byte->hex-string 255)

=>FF



(hex-string->byte "FF")

=> 255


(integer->hex-char 15)

=> #\F



(hex-char->integer #\F)

=> 15


(byte-add 1 3) => 1

(byte-or 1 2) => 3

(byte-xor 1 2) => 3

(byte>> 3 1) => 1

(byte<< 1 1) => 2
