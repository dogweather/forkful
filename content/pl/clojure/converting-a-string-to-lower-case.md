---
title:                "Clojure: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w pracy z danymi, może się zdarzyć że będziemy musieli dokonać konwersji napisu na małe litery. Powodem może być np. porównywanie napisów, czy zamiana dużych liter na małe w celu spójności danych. W tym przypadku, konwersja do małych liter jest niezbędna.

## Jak to zrobić

W celu konwersji napisu na małe litery w języku Clojure, musimy użyć funkcji `lower-case` z biblioteki `clojure.string`. Przykładowy kod wyglądałby następująco:

```Clojure
(require '[clojure.string :as str])

(str/lower-case "PRZYKŁADOWY NAPIS") ; wynik: "przykładowy napis"
```

Możemy również zastosować tę funkcję do kolekcji napisów, dzięki czemu cała kolekcja zostanie automatycznie przekonwertowana do małych liter.

```Clojure
(str/lower-case ["ABRA", "KADABRA"]) ; wynik: ["abra", "kadabra"]
```

## Głębsza analiza

Podczas przekonwertowania napisu na małe litery, warto pamiętać o kilku ważnych zagadnieniach. Po pierwsze, funkcja `lower-case` zwraca nowy napis, a nie zmienia oryginalnego. Dlatego, jeśli chcemy zastosować zmiany bezpośrednio na oryginalnym napisie, musimy skorzystać z funkcji `str/lower-case!`.

Należy również zwrócić uwagę na język, w którym jest zapisany dany napis. Niektóre znaki mogą mieć odmienne formy w zależności od języka, dlatego konwersja do małych liter może nie być skuteczna w przypadku napisów z różnymi alfabetami.

## Zobacz także

- Dokumentacja funkcji `str/lower-case`: https://clojuredocs.org/clojure.string/lower-case
- Porównywanie napisów w języku Clojure: https://code-maven.com/compare-strings-clojure