(ns btc.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn byte-seq [path]
  (letfn [(make-byte-seq [stream]
            (lazy-seq
             (cons (.read stream)
                   (make-byte-seq stream))))]
    (make-byte-seq (io/input-stream (io/file path)))))

(defn bytes-to-integer
  "Decode little-endian byte sequence of length <= 8 to an integer."
  [bytes]
  (.getLong ; Use long because the bytes represent an unsigned int
   (ByteBuffer/wrap (byte-array (concat
                                 (for [i (range (- 8 (count bytes)))
                                       :let [pad 0]] pad)
                                 (reverse bytes))))))

(defn bytes-to-hash [thirty-two-bytes]
  (apply str (for [byte (reverse thirty-two-bytes)] (format "%x" byte))))

(defn decode-block-header [byte-vec]
  {:version (bytes-to-integer (subvec byte-vec 0 4))
   :hash-prev-block (bytes-to-hash (subvec byte-vec 4 36))
   :hash-merkle-root (bytes-to-hash (subvec byte-vec 36 68))
   :time (new java.util.Date
              (* (bytes-to-integer (subvec byte-vec 68 72)) 1000))
   :bits (bytes-to-integer (subvec byte-vec 72 76))
   :nonce (bytes-to-integer (subvec byte-vec 76 80))})

(defn variable-int-size [first-byte]
  (case first-byte 0xFD 3 0xFE 5 0xFF 9 1))

(defn decode-inputs [byte-vec num-inputs]
  (when (> num-inputs 0)))

(defn transactions-size
  "Number of bytes a list of transactions consume."
  [transactions]
  0)

(defn decode-transactions [byte-vec]
  (let [in-counter-size (variable-int-size (nth byte-vec 7))
        in-counter (bytes-to-integer
                    (if (= in-counter-size 1)
                      (subvec byte-vec 6 7)
                      (subvec byte-vec 7 (+ 6 in-counter-size))))
        inputs (decode-inputs (subvec byte-vec 7) in-counter)
        inputs-size (transactions-size inputs)
        out-counter-loc (+ 6 in-counter-size inputs-size)
        out-counter-size (variable-int-size (nth byte-vec out-counter-loc))
        out-counter (bytes-to-integer
                     (if (= out-counter-size 1)
                      (subvec byte-vec out-counter-loc (inc out-counter-loc))
                      (subvec byte-vec (inc out-counter-loc)
                              (+ out-counter-loc out-counter-size))))]
    {:version (bytes-to-integer (subvec byte-vec 0 4))
     :flag (bytes-to-integer (subvec byte-vec 4 6))
     :in-counter in-counter
     :inputs nil
     :out-counter (subvec byte-vec 0 10)}))

(defn decode-block-bytes [byte-vec]
  (let [magic-no (format "%x" (bytes-to-integer (subvec byte-vec 0 4)))
        blocksize (bytes-to-integer (subvec byte-vec 4 8))
        blockheader (subvec byte-vec 8 88)
        transaction-counter-size (variable-int-size (nth byte-vec 88))]
    {:magic-no magic-no
     :blocksize blocksize
     :blockheader (decode-block-header blockheader)
     :transaction-counter (bytes-to-integer
                           (if (= transaction-counter-size 1)
                             (subvec byte-vec 88 89)
                             (subvec
                              byte-vec 89 (+ 88 transaction-counter-size))))
     :transactions (decode-transactions
                    (subvec byte-vec (+ 88 transaction-counter-size)))}))

(defn block-seq [path]
  (letfn [(blocks [the-byte-seq]
            (let [blocksize (bytes-to-integer (take 4 (drop 4 the-byte-seq)))]
              (lazy-seq
               (cons (decode-block-bytes
                      (vec (take (+ 4 4 blocksize) the-byte-seq)))
                     (blocks (drop (+ 4 4 blocksize) the-byte-seq))))))]
    (blocks (byte-seq path))))

(defn test []
  (block-seq  "../data/blk00000.dat"))
