(ns cinfix.tokenizer-test
  (:use clojure.test
        [cinfix.tokenizer] ))

(deftest tokenize-test
  (testing "2文字優先で記号の分離"
    (is (= (tokenize "-+*/***=>=<==!=") "( - + * / ** * = >= <= = !=)")))
  (testing "ankと記号は分離"
    (is (= (tokenize "asdf-jkl+qwert**yuiop") "( asdf - jkl + qwert ** yuiop)")))
  (testing "漢字の類いの解釈はank扱い"
    (is (= (tokenize "漢字asdf+jklの類い") "( 漢字asdf + jklの類い)")))
  (testing "punctの内_はankで"
    (is (= (tokenize "_first+_second") "( _first + _second)")))
  (testing "内側の括弧はリストに見えるように"
    (is (= (tokenize "asdf+jkl*(1+2)") "( asdf + jkl * ( 1 + 2 ))"))
    (is (= (tokenize "asdf+jkl[1+2]") "( asdf + jkl [ 1 + 2 ])"))))

