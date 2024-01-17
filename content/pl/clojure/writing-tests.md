---
title:                "Pisanie testów"
html_title:           "Clojure: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Czym i dlaczego? 
Pisanie testów to proces, w którym programiści tworzą specjalny kod, aby sprawdzić, czy ich program działa poprawnie. Jest to ważny element procesu tworzenia oprogramowania, ponieważ daje pewność, że nasz kod jest odporny na błędy i zmiany.

## Jak to zrobić: 
```Clojure 
(ns test (:require [clojure.test :refer [is]]))

(defn add [x y]
  (+ x y))

(deftest test-add
  (is (= 3 (add 1 2))))

(test-all) 
```

W powyższym przykładzie użyliśmy biblioteki Clojure Test, aby napisać prosty test funkcji dodającej, a następnie uruchomiliśmy wszystkie nasze testy. Jeśli nasz kod byłby niepoprawny, test zakończyłby się niepowodzeniem i dał nam informacje o błędzie.

## Głębszy zanurzenie: 
Pisanie testów jest istotne, ponieważ pozwala na szybsze wykrywanie i naprawianie błędów w naszym oprogramowaniu. Jest to również kluczowy element w praktykach programowania takich jak Test Driven Development (TDD) i Behavior Driven Development (BDD). Alternatywami dla Clojure Test są inne biblioteki, takie jak Speclj czy Midje. Testy w Clojure mogą być także tworzone przez programistów używając dostępnych funkcji assert i is, jednak biblioteki takie jak Clojure Test oferują bogatsze i bardziej wygodne metody testowania.

## Zobacz także: 
- Poradnik Clojure Test: https://clojure.org/guides/test
- Inne biblioteki testowania w języku Clojure: https://clojure.org/community/testing_tools
- Wprowadzenie do TDD w Clojure: https://code.tutsplus.com/tutorials/an-introduction-to-test-driven-development-in-clojure--cms-20307