---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyszukiwanie i zastępowanie tekstu to podstawowe operacje, które programiści wykonują na ciągach znaków. Ułatwiają one m.in. refaktoryzację kodu, naprawę błędów i manipulowanie danymi.

## Jak to zrobić:
Priorytetem Clojure jest czytelność i efektywność, a jego biblioteka standardowa udostępnia wiele rozwiązań umożliwiających manipulację tekstem. Użyj funkcji `clojure.string/replace` do wyszukiwania i zastępowania tekstów.

```Clojure
(require '[clojure.string :as str])

(def text "Witaj, Świecie!")

(println (str/replace text "Świecie" "Clojure"))
```
Po uruchomieniu kodu powyżej, otrzymamy następujący wynik:
```Clojure
Witaj, Clojure!
```

## Głębsze zanurzenie:
Historia: Clojure, jako język funkcyjny na platformie Java, został zaprojektowany z myślą o łatwym i efektywnym przetwarzaniu danych.

Alternatywy: Można użyć również wyrażeń regularnych, które są mocnym narzędziem do manipulowania tekstem, ale może być trudniejsze do opanowania.

Szczegóły implementacji: `clojure.string/replace` to uniwersalna funkcja, która jest częścią biblioteki standardowej Clojure. Działa na zasadzie wzorca (stringa lub wyrażenia regularnego) i zamienia wszystkie jego wystąpienia w tekście docelowym.

## Zobacz również:
1. Dokumentacja Clojure na temat [manipulacji tekstem](https://clojuredocs.org/clojure.string)
2. Dobry tutorial o [wyrażeniach regularnych w Clojure](https://www.regular-expressions.info/clojure.html)
3. Poradnik [Refactoring w Clojure](https://dev.to/petrussola/refactoring-in-clojure-with-cognitects-refactor-nrepl-and-emacs-90a) - przykład użycia funkcji replace w praktyce.