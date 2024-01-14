---
title:    "Clojure: Pisanie testów"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie kodu jest nieodłączną częścią procesu programowania. Pomaga zapewnić, że nasz kod działa poprawnie, a także ułatwia zmiany i rozwój naszej aplikacji. Jest to również ważny element wydajnego i niezawodnego tworzenia oprogramowania.

## Jak to zrobić?

Aby napisać testy w Clojure, musimy najpierw zainstalować narzędzie o nazwie `lein-test-refresh`. Możemy to zrobić za pomocą polecenia `lein plugin install lein-test-refresh`. Następnie, w naszym projekcie, musimy dodać poniższą linię do pliku `project.clj`:

```
:profiles {:dev {:dependencies [[org.clojure/test.check "RELEASE"]
                                [org.clojure/test.check.clj "RELEASE"]]}}
```

Teraz, możemy utworzyć plik `test/core_test.clj`, w którym będziemy umieszczać nasze testy. Przykładowy test może wyglądać tak:

```Clojure
(ns my-project.core-test
 (:require [clojure.test :refer :all]
           [my-project.core :as core]
           [clojure.test.check.generators :as gen]))

;; Testowanie funkcji dodawania
(deftest test-addition
  (is (= 5 (core/addition 2 3)))
  (is (= -2 (core/addition -5 3)))
  (is (= 10 (core/addition 0 10))))

;; Testowanie funkcji mnożenia z użyciem generatora liczb całkowitych
(deftest test-multiply
  (is (= 8 (core/multiply (* 2 -4))))
  (is (= 25 (core/multiply 5 -5))))
  (is (= 0 (core/multiply 23153 0)))
  (is (= -100 (core/multiply 10 -10))))

;; Testowanie funkcji z użyciem własnego generatora
(deftest test-custom-generator
  (for-all [x (gen/elements [:a :b :c :d])]
    (is (= x (core/custom-function x)))))

;; Uruchamiamy testy automatycznie za pomocą `lein test-refresh`
;; Testy będą się automatycznie uruchamiać przy każdej zmianie w kodzie
;; i pokażą nam wyniki w czasie rzeczywistym.
```

## Głębszy zanurzanie się

Aby napisać dobre testy, warto poznać narzędzie `clojure.test`. Pozwala ono na definiowanie asercji za pomocą funkcji `is`, `testing` oraz `are`. Możemy również wykorzystać w nim generatora testów `clojure.test.check` do testowania naszych funkcji z różnymi zestawami danych.

Poza tym, warto pamiętać, aby testować swoje funkcje nie tylko dla przypadków typowych, ale również dla skrajnych wartości oraz niepoprawnych danych. Testy powinny mieć za zadanie sprawdzić, czy nasz kod jest odporny na błędy i niezgodne dane. 

## Zobacz także

- [Dokumentacja Clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Dokumentacja Clojure.test.check](https://clojure.github.io/test.check/)
- [Przykładowy projekt z testami w Clojure](https://github.com/thma/Test-Driven-Clojure-Book-Code)
- [Poradnik Test-Driven Development w Clojure](https://devbridge.com/articles/clojure-test-driven-development/)