(ns lens.core
  (:use [clojure.set :only (intersection difference)]
        [clojure.string :only (join capitalize)]
        [clojure.java.io :only (resource)]
        [clojure.contrib.duck-streams :only (read-lines)])
  (:import [java.io File]))

;;;; TODO
;;;; simplify
;;;; encode/decode multimethod w/type dispatch
;;;; generic, swappable weighting system (markov, word freq)
;;;; actually use a byte type for the word mappings
;;;; simplify parse, maybe taking advantage of merge's overwrite

;;;; Word Mappings

(defn- read-words [file]
  "Reads a WordNet resource file from the classpath
   returning a seq of simple words."
  (for [line (read-lines (resource file))
        :when (and (Character/isLetter (first line)) ; skip header
                   (not (.contains line "_"))        ; skip collocations
                   (not (.contains line "-"))
                   (not (.contains line ".")))]      ; skip acronyms
    (first (.split line " "))))

;;; Below we partition using a byte range, but it should be noted
;;; that the actual numeric instances are int for simpler fn interop.

(let [num-partitions (Math/pow 2 Byte/SIZE)
      
      partition-size (fn [words]
                       (quot (count words)
                             num-partitions))
      
      truncate-extra (fn [words]
                       (take (* (partition-size words)
                                num-partitions)
                             words))]

  (defn- words->bytes [words]
    "Maps a word to its equivalent byte value."
    (zipmap (truncate-extra words)
            (mapcat (partial repeat (partition-size words))
                    (iterate inc Byte/MIN_VALUE))))

  (defn- bytes->words [words]
    "Maps a byte value to its equivalent coll of words."
    (zipmap (iterate inc Byte/MIN_VALUE)
            (partition (partition-size words)
                       (truncate-extra words))))) 

(defn- parse [{existing :all, :as aggregate} pos file]
  (let [raw-words    (set (read-words file))
        duplicates   (intersection (set (keys existing)) raw-words)
        unique-words (difference raw-words duplicates)
        sorted-words (sort unique-words)
        w->b-mapping (words->bytes sorted-words)
        b->w-mapping (bytes->words sorted-words)]
    (-> aggregate
        (update-in [:all] merge w->b-mapping)
        (assoc pos b->w-mapping))))

(letfn [(map-vals-to-nil-in
         [m to]
         (assoc m to (zipmap (-> m vals flatten)
                             (repeat nil))))]
  
  (def ^{:private true}
    dictionary (-> {:prep    ["to" "in" "by" "with" "on"]
                    :article ["the" "a"]
                    :name    ["Rob" "Lucka"]
                    :pronoun ["he" "she" "it" "these" "those" "that"]}
                   (map-vals-to-nil-in :all)
                   (parse :noun "index.noun")
                   (parse :verb "index.verb")
                   (parse :adj  "index.adj")
                   (parse :adv  "index.adv"))))

(letfn [(pos->word  [pos]   (rand-nth (dictionary pos)))
        (byte->word [pos n] (rand-nth (get-in dictionary [pos (int n)])))]
  
  (defn- get-word [pos bytes]
    "Retrieves a word for this part-of-speech, consuming a byte if possible."
    (if-let [word (byte->word pos (first bytes))]
      [word (next bytes)]
      [(pos->word pos) bytes])))

(defn- get-byte [word]
  (get-in dictionary [:all word]))


;;;; Sentence Generation

(def ^{:private true
       :doc "Defines grammar rules without the terminals."}
  grammar {:sentence    [:noun-phrase :verb-phrase]
           :noun-phrase '([:article :adj* :noun :pp*] [:name] [:pronoun])
           :verb-phrase [:verb :noun-phrase :pp*]
           :pp*         '(nil [:pp :pp*])
           :adj*        '(nil [:adj :adj*])
           :pp          [:prep :noun-phrase]})

(defn- generate
  "Generates a random sentence composed of terminal placeholders."
  ([]
     (generate :sentence))
  ([phrase]
     (->> (cond (list?   phrase) (generate (rand-nth phrase))
                (vector? phrase) (mapcat generate phrase)
                (grammar phrase) (generate (grammar phrase))
                :else            [phrase])
          (filter identity)))) ; remove nil terminals


;;;; Encoding / Decoding

(defn encode
  ([bytes]
     (encode (seq bytes) (generate) []))
  ([bytes sentence text]
     (if bytes
       (let [sentence     (or sentence (generate))
             [word bytes] (get-word (first sentence) bytes)]
         (recur bytes (next sentence) (conj text word)))
       (str (join " " (update-in text [0] capitalize)) "."))))

(defn decode
  ([text]
     (decode (re-seq #"\b\w+\b" text) []))
  ([words bytes]
     (if words
       (recur (next words) 
              (if-let [n (get-byte (first words))]
                (conj bytes (byte n))
                bytes))
       bytes)))


