;; From https://hackernoon.com/learn-blockchains-by-building-one-117428612f46
(ns blockchain.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as string]
            [digest :as digest]))

(defn sha [x]
  (digest/sha-256 (pr-str x)))

(s/def :bc/sha
  (s/with-gen
    (s/and string?
           #(re-matches #"[A-Fa-f0-9]+" %))
    #(s/gen (into #{} (map sha (range 10))))
    ))

(s/def :bc/sender :bc/sha)
(s/def :bc/recipient :bc/sha)
(s/def :bc/amount number?)
(s/def :bc/proof pos-int?)
(s/def :bc/prev-hash :bc/sha)

(s/def :bc/transaction (s/keys :req
                               [:bc/sender
                                :bc/recipient
                                :bc/amount]))
(s/def :bc/transactions (s/coll-of :bc/transaction
                                   :kind vector?))

(s/def :bc/index nat-int?)
(s/def :bc/timestamp inst?)
(s/def :bc/proof pos-int?)
(s/def :bc/prev-hash :bc/sha)

(s/def :bc/block (s/keys :req
                         [:bc/index
                          :bc/timestamp
                          :bc/transactions
                          :bc/proof
                          :bc/prev-hash]))

(s/def :bc/chain (s/coll-of :bc/block :kind vector?))

(s/def :bc/bc (s/keys :req [:bc/transactions
                            :bc/chain]))

(def genesis {:bc/prev-hash "1"
              :bc/proof 100})

(defn now [] (new java.util.Date))

(s/fdef add-block
        :args (s/cat :bc :bc/bc
                     :proof :bc/proof
                     :prev-hash :bc/prev-hash)
        :ret :bc/bc)
(defn add-block [bc proof prev-hash]
  (update bc
          :bc/chain
          conj
          {:bc/index (+ (count (:bc/chain bc)) 1)
           :bc/timestamp (now)
           :bc/transactions (:bc/transactions bc)
           :bc/proof proof
           :bc/prev-hash (or prev-hash (sha (last (:bc/chain bc))))}))

(s/fdef blockchain
        :ret :bc/bc)
(defn blockchain []
  (add-block
   {:bc/transactions []
    :bc/chain []}
   (:bc/proof genesis)
   (:bc/prev-hash genesis)))

(s/fdef add-tx
        :args (s/cat :bc :bc/bc
                     :sender :bc/sender
                     :recipient :bc/recipient
                     :amount :bc/amount)
        :ret :bc/bc)
(defn add-tx [bc sender recipient amount]
  (update bc
          :bc/transactions
          conj
          {:bc/sender sender
           :bc/recipient recipient
           :bc/amount amount}))

(s/fdef next-idx
        :args (s/cat :bc :bc/bc)
        :ret :bc/index)
(defn next-idx [bc]
  (-> bc
      :bc/chain
      last
      :bc/indix
      (or 0)))

(s/fdef proof-of-work
        :args (s/cat :last-proof :bc/proof)
        :ret :bc/proof)
(defn proof-of-work
  "Simple Proof of Work Algorithm:
   - Find a number p' such that hash(pp') contains leading 4 zeroes, where p is the previous p'
   - p is the previous proof, and p' is the new proof"
  [last-proof]
  (->> (range)
       (filter #(string/ends-with? (sha (str last-proof %)) "0000"))
       first))

(defn random-uuid [] (str (java.util.UUID/randomUUID)))

(defn node-id []
  (-> (random-uuid)
                    str
                    (string/replace "-" "")
                    )
  )

(comment
  (do
    (require '[clojure.spec.test.alpha])
    (clojure.spec.test.alpha/instrument))
  )

(comment
  (add-tx {:bc/chain []
           :bc/transactions []} "abc" "def" 5))

(comment (require '[orchestra.spec.test :as st])
         (set! s/*explain-out* expound/printer)
         (st/instrument))



(comment
  (def server-id (node-id))

  
  )

