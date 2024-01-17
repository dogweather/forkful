---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Clojure: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Usuwanie znaków pasujących do wzorca to niewielka, ale przydatna funkcja, która pozwala programistom na szybkie i precyzyjne przetwarzanie tekstów. Może to być potrzebne, gdy chcemy usunąć specyficzne znaki z tekstu, np. w celu wyciągnięcia tylko liczb lub słów kluczowych.

## Jak to zrobić:
```Clojure
(let [text "1,235.57 tekst 8,899.87"
      new-text (clojure.string/replace text #"\d+" "")]
  (println new-text))

;; Output: tekst
```
W tym przykładzie używamy funkcji `replace` z biblioteki `clojure.string`, aby usunąć wszystkie sekwencje cyfr z tekstu. Możemy także użyć wyrażenia regularnego, aby dopasować bardziej skomplikowane wzorce i zastąpić je pustym ciągiem znaków.

## Głębsze zagadnienia:
Usuwanie znaków dopasowujących do wzorca jest często wykorzystywane przy przetwarzaniu danych lub generowaniu raportów. Często używamy wyrażeń regularnych do dokładnego określenia, które znaki chcemy usunąć. W alternatywnej metodzie możemy używać funkcji `filter` do podziału tekstu na pojedyncze znaki i wykluczania tych, które pasują do wzorca. Wewnętrznie, wyrażenia regularne są kompilowane do wyrażeń regulowanych przez maszyny stanowe.

## Zobacz również:
- [Dokumentacja biblioteki clojure.string](https://clojuredocs.org/clojure.string)
- [Wyrażenia regularne w Clojure](https://clojuredocs.org/clojure.repl/source/clojure.repl/source/regexps.clj#%5E%5B%5E%3Aprivate%20%5D)
- [Poradnik wyrażeń regularnych](https://regexr.com/)