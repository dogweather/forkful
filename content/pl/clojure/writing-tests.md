---
title:    "Clojure: Pisanie testów"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest nieodłączną częścią programowania, a pisanie testów jest ważnym aspektem procesu tworzenia oprogramowania. Pomaga to w zapewnieniu jakości kodu, wykrywaniu błędów i ułatwia późniejsze wprowadzanie zmian do kodu. Dlatego warto się nauczyć, jak pisać testy w Clojure.

## Jak to zrobić

Testowanie w Clojure jest bardzo proste i wygodne, ponieważ język ten posiada wbudowane narzędzia do pisania testów, tzw. Clojure.test. Aby zacząć pisać testy, wystarczy zaimportować bibliotekę Clojure.test i zdefiniować swoje testy za pomocą makra `deftest`.

```Clojure
(ns mojProjekt.test
  (:require [clojure.test :refer :all])
  (:require [mojProjekt.core :refer :all]))

(deftest test-czy-liczby-dodatnie
    (is (liczba-dodatnia? 10))
    (is (liczba-dodatnia? 0))
    (is-not (liczba-dodatnia? -5))
)
```

W powyższym przykładzie zdefiniowaliśmy test `test-czy-liczby-dodatnie`, który sprawdza, czy funkcja `liczba-dodatnia?` zwraca poprawne wyniki dla różnych argumentów. Testy można również grupować w odpowiednie konteksty za pomocą makra `deftest`.

```Clojure
(deftest moje-testy
  (testing "liczby dodatnie"
    (is (liczba-dodatnia? 10))
    (is (liczba-dodatnia? 0))
    (is-not (liczba-dodatnia? -5)))
  (testing "liczby ujemne"
    (is (liczba-ujemna? -10))
    (is (liczba-ujemna? 0))
    (is-not (liczba-ujemna? 5)))
)
```

Po zdefiniowaniu testów, możemy uruchomić je za pomocą funkcji `run-tests`:

```Clojure
(run-tests 'mojProjekt.test)
```

Po wykonaniu testów otrzymamy informację o tym, które testy zostały zakończone pozytywnie, a które nie. Jest to bardzo wygodny sposób na sprawdzenie poprawności naszego kodu.

## Głębsza analiza

Istnieje wiele innych funkcji i makr dostępnych w bibliotece Clojure.test, które pozwalają na precyzyjne definiowanie i uruchamianie testów. Można również tworzyć własne asercje, które ułatwią pisanie testów.

Pamiętaj również, że pisanie testów nie zastępuje dokładnego sprawdzania kodu, ale jest jedynie dodatkowym narzędziem, które pomaga w tworzeniu stabilnego i niezawodnego oprogramowania.

## Zobacz także

- [Dokumentacja biblioteki Clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Jak pisać testy w Clojure z Clojure.test](https://lkrnac.net/blog/2017/04/clojure-test/)
- [Poradnik dla początkujących w testowaniu w Clojure](https://medium.com/@mrtylerbennett/beginners-guide-to-testing-in-clojure-805b74102c10)