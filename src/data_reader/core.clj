(ns btc.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn bytes-to-int
  "Decode (little-endian) byte-seq of length <= 8 to an integer."
  [byte-seq]
  (.getLong ; Use long because the bytes represent an unsigned int
   (ByteBuffer/wrap (byte-array (concat (for [i (range (- 8 (count byte-seq)))
                                              :let [pad 0]] pad)
                                        (reverse byte-seq))))))

(defn bytes-to-bigint
  "Decode (little-endian) 32 byte sequence to a BigInteger."
  [thirty-two-bytes]
  (new java.math.BigInteger (byte-array (reverse thirty-two-bytes))))

(defn bytes-to-hex
  "Decode a sequence of bytes into hex."
  [byte-seq]
  (apply str (map (fn [byte] (format "%02x" byte)) byte-seq)))

(defn next-var-int-desc
  "Return map with :value and :size of next variable-length int in byte-vec."
  [byte-vec]
  (let [size (case (nth byte-vec 0) 0xFD 3 0xFE 5 0xFF 9 1)]
    {:value (bytes-to-int
             (if (= size 1) (subvec byte-vec 0 1) (subvec byte-vec 1 size)))
     :size size}))

(defn decode-block-header
  "Decode 80 bytes of bitcoin header data into a map."
  [byte-vec]
  {:version (bytes-to-int (subvec byte-vec 0 4))
   :hash-prev-block (bytes-to-bigint (subvec byte-vec 4 36))
   :hash-merkle-root (bytes-to-bigint (subvec byte-vec 36 68))
   :time (bytes-to-int (subvec byte-vec 68 72))
   :bits (bytes-to-int (subvec byte-vec 72 76))
   :nonce (bytes-to-int (subvec byte-vec 76 80))})

(defn decode-inputs
  "Decode input-count number of inputs from byte-vec."
  [byte-vec input-count]
  (letfn [(next-input [byte-vec]
            (let [script-len-desc (next-var-int-desc (subvec byte-vec 36))
                  script-start (+ 36 (:size script-len-desc))
                  script-end (+ script-start (:value script-len-desc))]
              {:previous-transaction-hash (bytes-to-bigint
                                           (subvec byte-vec 0 32))
               :previous-txout-index (bytes-to-int (subvec byte-vec 32 36))
               :tx-script-length (:value script-len-desc)
               :tx-script (bytes-to-hex
                           (subvec byte-vec script-start script-end))
               :sequence-no (bytes-to-int
                             (subvec byte-vec script-end (+ script-end 4)))
               :size (+ script-end 4)}))]
    (when (> input-count 0)
      (let [nxt-in (next-input byte-vec)]
        (cons nxt-in (decode-inputs (subvec byte-vec (:size nxt-in))
                                    (dec input-count)))))))

(defn decode-outputs
  "Decode output-count number of outputs from byte-vec."
  [byte-vec output-count]
  (letfn [(next-output [byte-vec]
            (let [script-len-desc (next-var-int-desc (subvec byte-vec 8))
                  script-start (+ 8 (:size script-len-desc))
                  script-end (+ script-start (:value script-len-desc))]
              {:value (bytes-to-int (subvec byte-vec 0 8))
               :tx-out-script-length (:value script-len-desc)
               :tx-out-script (bytes-to-hex
                               (subvec byte-vec script-start script-end))
               :size script-end}))]
    (when (> output-count 0)
      (let [nxt-out (next-output byte-vec)]
        (cons nxt-out (decode-outputs (subvec byte-vec (:size nxt-out))
                                      (dec output-count)))))))

(defn decode-transactions
  "Decode num-trans number of transactions from byte-vec."
  [byte-vec num-trans]
  (when (> num-trans 0)
    (let [in-count-desc (next-var-int-desc (subvec byte-vec 4))
          inputs (decode-inputs
                  (subvec byte-vec (+ 4 (:size in-count-desc)))
                  (:value in-count-desc))
          inputs-size (apply + (map :size inputs))
          out-count-desc (next-var-int-desc
                          (subvec byte-vec
                                  (+ 4 (:size in-count-desc) inputs-size)))
          outputs (decode-outputs
                   (subvec byte-vec (+ 4
                                       (:size in-count-desc)
                                       inputs-size
                                       (:size out-count-desc)))
                   (:value out-count-desc))]
      (cons
       {:version (bytes-to-int (subvec byte-vec 0 4))
        ;; :flag (bytes-to-int (subvec byte-vec 4 6))
        :in-counter (:value in-count-desc)
        :list-of-inputs inputs
        :out-counter (:value out-count-desc)
        :list-of-outputs outputs
        :lock-time (bytes-to-int (vec (take-last 4 byte-vec)))}
       (decode-transactions (subvec byte-vec 0) (dec num-trans))))))

(defn decode-block-bytes
  "Decode byte-vec into block map."
  [byte-vec]
  (let [magic-no (bytes-to-int (subvec byte-vec 0 4))
        blocksize (bytes-to-int (subvec byte-vec 4 8))
        blockheader (subvec byte-vec 8 88)
        transact-desc (next-var-int-desc (subvec byte-vec 88))]
    {:magic-no magic-no
     :blocksize blocksize
     :blockheader (decode-block-header blockheader)
     :transaction-counter (:value transact-desc)
     :transactions (decode-transactions
                    (subvec byte-vec (+ 88 (:size transact-desc)))
                    (:value transact-desc))
     :witnesses nil
     :lock-time nil}))

(defn read-block
  "Read one block of data from blk-file at path, starting at position."
  [path position]
  (with-open [f (java.io.RandomAccessFile. path "r")]
    (let [byte-holder (byte-array 4)]
      (.seek f (+ position 4)) ; Jump to blocksize
      (.readFully f byte-holder 0 4) ; Read in four bytes
      (let [blocksize (+ 8 (bytes-to-int byte-holder))
            block-bytes (byte-array blocksize)]
        (.seek f position) ; Jump back to block start
        (.readFully f block-bytes 0 blocksize)
        (decode-block-bytes (vec block-bytes))))))
 
(defn test-read []
  (read-block "../data/blk00000.dat" 293))
