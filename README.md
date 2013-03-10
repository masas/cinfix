# cinfix
Cinfix is a simple library providing infix notation in Clojure.

## Usage
[cinfix "0.1.0"]

    ;; 
    => ($= 1 + 2 + 3 * 4)
    15

    ;; arrayWith[index] 
    => (let [a ["a" "b" "
          (str ($= a[0]) ($= a[1]) ($= a[2])))
     "abc"

    ;; Math/ function
    => ($= sin(45 / 180.0 * Math/PI) * sin(45 / 180.0 * Math/PI))
    0.4999999999999999

    ;; many arg func
    => ($= pow((2) (4)))
    16.0

## License

Distributed under the Eclipse Public License, the same as Clojure.
