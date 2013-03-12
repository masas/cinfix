(ns cinfix.core
  (:use cinfix.tokenizer))

;;
(def +operator-rank+
  {'&& {:priority 0 :join :left :replace 'and}
   '|| {:priority 0 :join :left :replace 'or}
   '=  {:priority 1 :join :left :replace '=}
   '!= {:priority 1 :join :left :replace 'not=}
   '>  {:priority 1 :join :left :replace '>}
   '<  {:priority 1 :join :left :replace '<}
   '>= {:priority 1 :join :left :replace '>=}
   '<= {:priority 1 :join :left :replace '<=}
   '+  {:priority 2 :join :left :replace '+}
   '-  {:priority 2 :join :left :replace '-}
   '*  {:priority 3 :join :left :replace '*}
   '/  {:priority 3 :join :left :replace '/}
   '%  {:priority 3 :join :left :replace 'mod} ;!!!
   '** {:priority 4 :join :right :replace 'Math/pow}})

(def +max-priolity+ (inc (apply max (map #(:priority %) (vals +operator-rank+)))))


;; Math/無しの為のマップ
(def +lose-math-name+
  {'abs 'Math/abs
   'acos 'Math/acos
   'asin 'Math/asin
   'atan 'Math/atan
   'atan2 'Math/atan2
   'cbrt 'Math/cbrt
   'ceil 'Math/ceil
   'copySign 'Math/copySign
   'cos 'Math/cos
   'cosh 'Math/cosh
   'exp 'Math/exp
   'expm1 'Math/expm1
   'floor 'Math/floor
   'getExponent 'Math/getExponent
   'hypot 'Math/hypot
   'IEEEremainder 'Math/IEEEremainder
   'log 'Math/log
   'log10 'Math/log10
   'log1p 'Math/log1p
   'max 'Math/max
   'min 'Math/min
   'nextAfter 'Math/nextAfter
   'nextUp 'Math/nextUp
   'pow 'Math/pow
   'random 'Math/random
   'rint 'Math/rint
   'round 'Math/round
   'scalb 'Math/scalb
   'signum 'Math/signum
   'sin 'Math/sin
   'sinh 'Math/sinh
   'sqrt 'Math/sqrt
   'tan 'Math/tan
   'tanh 'Math/tanh
   'toDegrees 'Math/toDegrees
   'toRadians 'Math/toRadians
   'ulp 'Math/ulp
   })

;;; +lose-math-name+をbindingしておくと関数呼び出しの処理でMath付きにして処理する
(def ^{:dynamic true}*lose-math-name* {} )


(defn indirect-access
  "idxとobjの値によってmapとしてかvectorとしてアクセスさせる。
varname[index]を扱う為の処理、とっても遅い"
  [obj idx]
  (cond
   (keyword? idx) (idx obj)
   (map? obj) (obj idx)
   true (nth obj idx)))

;; expressionは前方参照あり
(declare expression)

(defn- index-access
  "xsのfirstがvecであることが条件で呼び出され、varnameに対して間接参照を生成する"
  [varname xs]
  (assert (vector? (first xs)))
  (loop [result (list 'cinfix.core/indirect-access varname (expression (first xs))) rest-subscript (next xs)]
    (if (vector? (first rest-subscript))
      (recur (list 'cinfix.core/indirect-access result (expression (first rest-subscript))) (next rest-subscript))
      [result rest-subscript])))


(defn- maybe-funcall
  "name(arg)な関数として処理,s式中カンマが空白扱いなので複数の引数は name((arg-exp) (arg-exp))の様に書かないと駄目"
  [funcname s]
  (let [name (or (*lose-math-name* funcname) funcname) ]
    (if (empty? s)
      (list name )
      (if (seq? (first s))
        (cons name (map #(expression %) s))
        (list name (expression s))))))

(defn- maybe-value
  "値だと思うけど、名前の直後がseqなら関数呼び出しだと思って処理、名前の直後がvectorなら配列の様に解釈"
  [x xs]
  (let [prefetch-word (first xs)]
    (cond 
     (vector? prefetch-word) (index-access x xs)
     (seq? prefetch-word) [(maybe-funcall x prefetch-word) (next xs)]
     (seq? x) [(expression x) xs]
     true [x xs])))

(defn- simple-factor
  "単項演算子のマイナス記号だけちょっとずるをする"
  [x xs]
  (if (= x '-) 
    (let [[value rest] (maybe-value (first xs) (next xs)) ]
      [(list '- value) rest])
      (maybe-value x xs)))

(defn- simple-expression
  "a operator b の処理を再帰で処理してリストにする,戻り値は[変換済みs式 infix式の残りリスト]なペア"
  [priority x xs]
  (let [[ exp-list rest-tokens ]  (if (not= priority +max-priolity+)
                                    (simple-expression (inc priority) x xs)
                                    (simple-factor x xs))]
    (loop [ex exp-list lst rest-tokens]
      (let [op (first lst)
            rank (+operator-rank+ op)]
        (if (= priority (:priority rank))
          (if (= :left (:join rank))
            (let [[rhs n] (simple-expression (inc priority) (second lst) (nnext lst)) ]
              (recur (list (:replace rank) ex rhs) n))
            (let [[rhs n] (simple-expression 0 (second lst) (nnext lst))]
              [(list (:replace rank) ex rhs) n] ))
          [ex lst])))))

(defn expression
  "infix notation symbol list -> sexpression"
  [s]
  (if (and (= (count s) 1) (string? (first s)))
    (recur (read-string (tokenize (first s))))
    (let [[result rest-lst] (simple-expression 0 (first s) (next s))]
      (when (not (nil? rest-lst))
        (throw (ex-info "error" {:original-expression s :rest-data rest-lst :out-sexp result} ) ))
      result)))

(defmacro $$
  "分かち書きした普通の式をs式に変換する、各識別子はclojureのreaderで区分けされる必要があるのでa+bと a + bは異なる扱いをうける、array[idx],func(arg)みたいなのは[],()が処理してくれるので分かち書きの必要は無い。"
  [& exp]
  (expression exp))

(defmacro $=
  "$$のMath関数怠惰版"
  [& exp]
  (binding [*lose-math-name* +lose-math-name+]
    (expression exp)))
