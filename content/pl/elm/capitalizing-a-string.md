---
title:                "Elm: Zapisanie wielkich liter na ciągu znaków"
simple_title:         "Zapisanie wielkich liter na ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#####

Po co: Dlaczego warto sprawiać, aby łańcuch znaków był napisany wielką literą?

 Elm jest językiem programowania, który wykorzystuje statyczne typy, co oznacza, że kod jest bardziej bezpieczny i łatwiejszy w utrzymaniu. Jedną z przydatnych funkcji języka jest możliwość konwertowania dowolnego łańcucha znaków na napis napisany wielką literą. Jest to szczególnie przydatne w przypadku tworzenia aplikacji, w których ważne jest zachowanie jednolitej konwencji pisania.

## Jak to zrobić:

```elm
import String

text = "to jest przykładowy tekst"
capitalizedText = String.toUpper text

-- Output:
-- "TO JEST PRZYKŁADOWY TEKST"
```
W powyższym przykładzie importujemy moduł String, który dostarcza nam funkcji do operacji na łańcuchach znaków. Następnie tworzymy zmienną "text" zawierającą nasz łańcuch znaków, a po tym używamy funkcji "toUpper", która zwraca ten sam tekst, ale zapisany wielkimi literami. Takie proste rozwiązanie pozwala uniknąć konieczności ręcznego zmieniania wszystkich liter na wielkie, co jest nie tylko czasochłonne, ale również podatne na błędy.

## Deep Dive:

Funkcja "toUpper" jest jedną z wielu funkcji dostępnych w module String, który obsługuje szeroki zakres operacji na łańcuchach znaków. Można również użyć funkcji "toLower" do zamiany wszystkich liter na małe, lub "trim" do usunięcia białych znaków z początku i końca tekstu. Istnieje również wiele innych pomocnych funkcji, takich jak "reverse", "padLeft" czy "slice", które pozwalają na bardziej zaawansowane manipulacje tekstami.

## Zobacz także:

- Dokumentacja Modułu String w Elm - https://package.elm-lang.org/packages/elm/core/latest/String
- Inne przydatne funkcje w Elm - https://guide.elm-lang.org/appendix/functions.html
- Przykładowy projekt w Elm - https://github.com/elm/projects