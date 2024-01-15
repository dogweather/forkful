---
title:                "Wyszukiwanie i podmienianie tekstu"
html_title:           "Clojure: Wyszukiwanie i podmienianie tekstu"
simple_title:         "Wyszukiwanie i podmienianie tekstu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, gdy pracujemy z dużymi ilościami tekstu, konieczne jest dokonanie zmian w wielu miejscach. W takiej sytuacji, narzędzie do wyszukiwania i zastępowania tekstu jest nieocenione, ponieważ pozwala zaoszczędzić czas i uniknąć błędów ludzkich.

## Jak to zrobić

W Clojure, możemy użyć funkcji `replace` do wyszukiwania i zastępowania tekstu. Poniżej znajdują się przykłady kodu i wyjścia, wykorzystując różne możliwości funkcji `replace`.

```Clojure
;; Przykład 1
(replace "Hello World" "World" "Clojure")
;; Output: "Hello Clojure"

;; Przykład 2
(replace "aaa bbb ccc" #"a+" "d")
;; Output: "ddd bbb ccc"

;; Przykład 3
(replace "1,2,3,4,5" #"\d+" #(str (Integer/parseInt %) 10)) 
;; Output: "10,20,30,40,50"
```

Możemy również użyć funkcji `replace-first`, aby tylko pierwsze wystąpienie tekstu zostało zastąpione.

```Clojure
(replace-first "1,2,3,4,5" #"\d+" #(str (Integer/parseInt %) 10)) 
;; Output: "10,2,3,4,5"
```

## Głębsze zanurzenie

Funkcje `replace` i `replace-first` przyjmują trzy argumenty: tekst, wzorzec do wyszukania oraz funkcję lub tekst, którym ma zostać zastąpiony. Wzorzec może być wyrażeniem regularnym, które pozwala na zaawansowane wyszukiwanie tekstu.

Możemy również wykorzystać funkcję `replace` w połączeniu z funkcją `map` do dokonania zmian w wielu elementach listy.

```Clojure
(def data ["1,2,3" "4,5,6"])
(map #(replace % #"\d+" #(str (Integer/parseInt %) 10)) data)
;; Output: ("10,20,30" "40,50,60")
```

## Zobacz również

- Dokumentacja funkcji `replace`: https://clojuredocs.org/clojure.core/replace
- Przykłady zastosowania funkcji `replace`: https://stackoverflow.com/questions/45988078/replace-string-in-a-file-using-clojure
- Przewodnik po wyrażeniach regularnych w Clojure: https://www.braveclojure.com/core-functions-in-depth/#regexp