(ns cinfix.core-test
  (:use clojure.test
        [cinfix.core :only [$=]] ))

(deftest simple-expression-test
  (testing "普通の四則演算"
    (is (= ($= 2 + 3) 5))
    (is (= ($= 2 - 3) -1))
    (is (= ($= 2 * 3) 6))
    (is (= ($= 2 / 3) 2/3))))

(deftest simple-expression-with-variable-test
  (testing "四則演算を変数で"
    (let [x 10 y 5]
      (is (= ($= x + y) (+ x y)))
      (is (= ($= x - y) (- x y)))
      (is (= ($= x * y) (* x y)))
      (is (= ($= x / y) (/ x y))))))

(deftest operator-priority-test
  (testing "加減乗除で優先度をテスト"
    (is (= ($= 1 * 2 + 3 + 4) (+ (+ (* 1 2) 3) 4 )))
    (is (= ($= 1 + 2 * 3 + 4) (+ (+ 1 (* 2 3)) 4 )))
    (is (= ($= 1 + 2 + 3 * 4) (+ (+ 1 2) (* 3 4) )))
    
    (is (= ($= 1 * 2 - 3 - 4) (- (- (* 1 2) 3) 4 )))
    (is (= ($= 1 - 2 * 3 - 4) (- (- 1 (* 2 3)) 4 )))
    (is (= ($= 1 - 2 - 3 * 4) (- (- 1 2) (* 3 4) )))

    (is (= ($= 1 / 2 + 3 + 4) (+ (+ (/ 1 2) 3) 4 )))
    (is (= ($= 1 + 2 / 3 + 4) (+ (+ 1 (/ 2 3) 4) )))
    (is (= ($= 1 + 2 + 3 / 4) (+ (+ 1 2) (/ 3 4) )))

    (is (= ($= 1 / 2 - 3 - 4) (- (- (/ 1 2) 3) 4 )))
    (is (= ($= 1 - 2 / 3 - 4) (- (- 1 (/ 2 3)) 4 )))
    (is (= ($= 1 - 2 - 3 / 4) (- (- 1 2) (/ 3 4) )))

    (is (= ($= 2 * 3 + 4 * 5) (+ (* 2 3) (* 4 5))))))

(deftest parent-priority-test
  (testing "各個最優先"
    (is (= ($= 1 + (2 + 3) + 4) (+ (+ 1 (+ 2 3)) 4 )))
    (is (= ($= 1 * (2 + 3) + 4) (+ (* 1 (+ 2 3) ) 4 )))
    (is (= ($= 1 + (2 + 3) * 4) (+ 1 (* (+ 2 3) 4) )))

    (is (= ($= 1 - (2 - 3) - 4) (- (- 1 (- 2 3)) 4 )))
    (is (= ($= 1 * (2 - 3) - 4) (- (- (* 1 2) 3) 4 )))
    (is (= ($= 1 - (2 - 3) * 4) (- 1 (* (- 2 3) 4) )))

    (is (= ($= 1 / (2 + 3) + 4) (+ (/ 1 (+ 2 3)) 4 )))
    (is (= ($= 1 + (2 + 3) / 4) (+ 1 (/ (+ 2 3) 4) )))

    (is (= ($= 1 / (2 - 3) - 4) (- (/ 1 (- 2 3)) 4 )))
    (is (= ($= 1 - (2 - 3) / 4) (- 1 (/ (- 2 3) 4))))

    (is (= ($= 2 * (3 + 4) * 5) (* (* 2 (+ 3 4) ) 5) )))) 

(deftest power-operator-test
  (testing "2 ** 3 = pow(2,3)とか 2 ** 3 + 4 = pow(2,3+4)と **以降が先に算出されるか"
    (is (= ($= 2 ** 3) (Math/pow 2 3)))
    (is (= ($= 2 ** 3 + 4 * 5) (Math/pow 2 (+ 3 (* 4 5)) )))))

(deftest unary-minus-test
  (is (= ($= 10 * - 10) (* 10 -10)) )
  (is (= ($= 10 + - 10) 0))
  (testing "関数の結果とか、配列に触れても？"
    (let [array (vec (map #(* % %) (range 10))) ]
      (loop [i 0]
        (when (< i (count array))
          (is (= ($= 10 * - array [i]) (* 10 (- (nth array i)))))
          (is (= ($= 10 * - sin (i / 10.0 * Math/PI)) (* 10 (- (Math/sin (* (/ i 10.0) Math/PI ))) )))
          (recur (inc i)))))))

(deftest map-access-test
  (testing "[]でマップを触る"
    (let [mymap {:test "teststr" :foo "foostr" 10 :ten}]
      (is (= ($= mymap[:test]) "teststr"))
      (is (= ($= mymap[:foo]) "foostr"))
      (is (= ($= mymap[10]) :ten))
      (is (= ($= str((mymap[:test]), (mymap[:foo]))) "teststrfoostr" )))))

(deftest array-access-test
  (testing "[]で配列を触る"
    (let [my_array [5 4 3 2]] ;ハイフンでつなぐとinfix式中でキモイので_で
      (is (= ($= my_array[0]) 5))
      (is (= ($= my_array[1]) 4))
      (is (= ($= my_array[2]) 3))
      (is (= ($= my_array[3]) 2)))))

(defn- fcall
  "引数の数を文字列で返す４つの定義をもったテスト用"
  ([] "arg-count-0")
  ([a] (str "arg-count-1," a))
  ([a b] (str "arg-count-2," a "," b))
  ([a b c] (str "arg-count-3," a "," b "," c) ))

(deftest function-call-test
  (testing "引数の数が違うときの呼び出し、引数が2個以上は入れ子のリストで"
    (is (= ($= fcall())  "arg-count-0"))
    (is (= ($= fcall(1)) "arg-count-1,1"))
    (is (= ($= fcall((2),(3))) "arg-count-2,2,3"))
    (is (= ($= fcall((4),(5),(6))) "arg-count-3,4,5,6"))))