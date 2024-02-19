---
aliases:
- /pl/clojure/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:02.336315-07:00
description: "Pisanie test\xF3w w Clojure, podobnie jak w innych j\u0119zykach programowania,\
  \ polega na tworzeniu dedykowanego kodu, kt\xF3ry weryfikuje, czy g\u0142\xF3wna\
  \ baza kodu\u2026"
lastmod: 2024-02-18 23:08:49.268258
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Clojure, podobnie jak w innych j\u0119zykach programowania,\
  \ polega na tworzeniu dedykowanego kodu, kt\xF3ry weryfikuje, czy g\u0142\xF3wna\
  \ baza kodu\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów w Clojure, podobnie jak w innych językach programowania, polega na tworzeniu dedykowanego kodu, który weryfikuje, czy główna baza kodu działa zgodnie z oczekiwaniami. Pomaga to w zapewnieniu poprawności, ułatwia refaktoryzację oraz podnosi stabilność kodu.

## Jak to zrobić:
Clojure, wykorzystując JVM, obsługuje różne frameworki do testowania. Jednak powszechnie używaną wbudowaną biblioteką jest `clojure.test`. Oto prosty przykład:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Funkcjonalność dodawania"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Po uruchomieniu tego testu zobaczylibyście wynik podobny do:

```
Testowanie example.test

Uruchomiono 2 testy zawierające 2 asercje.
0 niepowodzeń, 0 błędów.
```

Dla tych, którzy szukają opcji z większą ilością funkcji, można wykorzystać biblioteki firm trzecich, takie jak `Midje` lub `test.check`. Oto jak można użyć Midje dla podobnego testu:

Najpierw dodaj Midje do zależności twojego project.clj:
```clojure
[midje "1.9.9"]
```

Następnie, twój test z Midje może wyglądać tak:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Testowanie dodawania"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Po uruchomieniu testu przez Midje za pomocą `lein midje`, wyjście miałoby wyświetlić coś w stylu:

```
Wszystkie kontrole (2) zakończone sukcesem.
```
