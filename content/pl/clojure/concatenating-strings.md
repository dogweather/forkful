---
title:                "Clojure: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinieneś używać konkatenacji stringów? Jest to ważny element w programowaniu Clojure, ponieważ pozwala na łączenie różnych stringów w jeden. W ten sposób możemy tworzyć dowolne wyrażenia i przetwarzać dane w sposób bardziej złożony.

## Jak to zrobić

Przykładowy kod wykorzystujący konkatenację stringów wygląda następująco:

```Clojure
(def imie "Kasia")
(def nazwisko "Nowak")
(def wiek 28)

(str "Cześć, jestem " imie " " nazwisko " i mam " wiek " lat.")
```

Wynikiem działania tego kodu będzie: `Cześć, jestem Kasia Nowak i mam 28 lat.`

Jedną z najczęstszych metod konkatenacji stringów w Clojure jest używanie funkcji `str`, która łączy wszystkie argumenty w jedną dłuższą linię. Możemy również użyć `concat`, aby połączyć listę stringów w jedną linię lub operatora `str` do łączenia dwóch stringów.

## Głębsza analiza

Podczas konkatenacji stringów w Clojure warto pamiętać o typach danych. Jeśli jeden z argumentów jest listą, to zostanie ona rozwinięta i połączona z resztą stringów. Jeśli argument jest liczbą, zostanie automatycznie przekształcony na string.

Inną ciekawą funkcjonalnością jest możliwość łączenia stringów za pomocą separatora przy użyciu `str-join` lub `join` (w przypadku list). Jest to przydatne w przypadku łączenia większej ilości wartości i chcemy oddzielić je od siebie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o konkatenacji stringów w Clojure, polecamy zapoznać się z poniższymi materiałami:

- https://www.tutorialspoint.com/clojure/clojure_strings.htm
- https://clojuredocs.org/clojure.core/str
- https://www.clojure.org/reference/strings