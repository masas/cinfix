(ns cinfix.tokenizer)
;;TODO 後で普通にStreamで一文字毎に処理するのも試す


;; この定義順でマッチしたものを切り出して空白でつなぎ分かち書きにする
;; 分かち書きの両側を"(" ")"にするとreaderで読めるリストになる
(def +re-2char-punct-symbol+  #"^(&&|\|\||[!><]=|\*\*)")
(def +re-bigdecimal-number+  #"^(\d+\.\d*[Ee][\-\+]?\d+|\d+\.\d*M)")
(def +re-double-number+  #"^(\d+\.\d*[Ee][\-\+]?\d+|\d+\.\d*)")
(def +re-bignum+  #"^(\d+[MN])")
(def +re-int-number+  #"^(\d+)")
(def +re-ident+  #"^(([^\d\p{Punct}]|_)([^\p{Punct}]|_)*)")
(def +re-open-parent+ #"^(\()")
(def +re-open-bracket+ #"^(\[)")
(def +re-punct-symbol+  #"^(\p{Punct})")

(defn- make-val
  [basestr first-match]
  [first-match (subs basestr (count first-match))])

(declare read-word)

(defn- read-seq
  "sをendstrがヒットするまで分解して返す"
  [^String s startstr endstr]
  (loop [result (list startstr) s (subs s (count startstr))]
    (if (empty? s)
      [(clojure.string/join " " (reverse result)) s] 
      (let [[w l] (read-word s)]
        (if (= w endstr)
          [(clojure.string/join " " (reverse (cons w result))) l] 
          (recur (cons w result) l))))))

(defn read-word
  [^String s]
  (let [trimed (clojure.string/triml s)]
    (loop [pattern [[+re-2char-punct-symbol+ make-val]
                    [+re-bigdecimal-number+ make-val] 
                    [+re-double-number+ make-val] 
                    [+re-bignum+ make-val] 
                    [+re-int-number+ make-val] 
                    [+re-ident+ make-val] 
                    [+re-open-parent+ #(read-seq %1 %2 ")") ] 
                    [+re-open-bracket+ #(read-seq %1 %2 "]")] 
                    [+re-punct-symbol+ make-val ]]]
      (if-let [[r f] (first pattern)]
        (if-let [m (re-find r trimed)]
          (f trimed (first m))
          (recur (rest pattern)))
        ["" trimed]))))

(defn tokenize
  "入力文字列を単語で分かち書きし、reader-stringで読める文字列を作って返す"
  [^String s]
  (str "(" (first (read-seq s "" "")) ")"))
