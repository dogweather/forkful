---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Testowanie to proces weryfikowania, czy nasz kod robi to, co powinien. Robimy to, by zapobiec błędom, oszczędzić czas na debugowaniu i ulepszyć jakość kodu.

## Jak to zrobić:
Clojure używa biblioteki `clojure.test` do pisania i uruchamiania testów. Przykład:

```Clojure
(require '[clojure.test :refer :all])

(deftest test-dodawania
  (testing "Czy 1 + 1 = 2?"
    (is (= 2 (+ 1 1)))))
    
(run-tests)
```

Output:

```
Testing user

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Wnikliwe informacje
Testowanie w Clojure to nie tylko `clojure.test`. Istnieje wiele opcji, jak `Midje` czy `Speclj`. Historia testowania w Clojure jest dość krótka, ale intensywna, odzwierciedla rosnący wymóg na wydajne, niezawodne systemy. Testy można uruchamiać ręcznie, przez REPL, lub automatycznie, co jest częstą praktyką Continuous Integration.

## Zobacz także
- [The Clojure Cheatsheet](https://clojure.org/api/cheatsheet) – kompendium funkcji Clojure, w tym testowych.
- [Clojure for the Brave and True](https://www.braveclojure.com/) - darmowy online podręcznik do Clojure, w tym rozdział o testowaniu.
- [Clojure Testing with Midje](https://github.com/marick/Midje/) - informacje o alternatywnej bibliotece do testowania Midje.
