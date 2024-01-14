---
title:    "Elm: Generowanie losowych liczb."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy wygenerować losowe liczby? W programowaniu istnieje wiele zastosowań dla generatorów liczb losowych, takich jak gry, symulacje, testy jednostkowe i wiele innych.

## Jak to zrobić

Możemy wykorzystać moduł Matematyka w języku Elm, aby wygenerować losowe liczby. W przykładzie poniżej używamy funkcji `generate` i `uniform` aby wygenerować 5 liczb losowych z zakresu od 0 do 10.

```Elm
import Matematyka exposing (generate, uniform)

losoweLiczby : List Int
losoweLiczby =
  generate
    (uniform 0 10)
    5
```

Przykładowy wynik: `[5, 2, 9, 1, 7]`

Możemy także użyć funkcji `list` w celu wygenerowania listy o określonej długości:

```Elm
import Matematyka exposing (list, uniform)

losoweLiczby : List Int
losoweLiczby =
  list 10
    (uniform 0 100)
```

Przykładowy wynik: `[99, 23, 45, 81, 12, 35, 67, 3, 55, 98]`

## Głębsza analiza

Generator liczb losowych w Elm jest oparty na generatorze liczb losowych znanym jako Mersenne Twister. Jest to bardzo wydajny i dokładny algorytm, który generuje liczbę o określonej długości seeda. W Elm, seed jest generowany na podstawie aktualnego czasu, co zapewnia różnorodność wygenerowanych liczb przy każdym uruchomieniu programu.

Ważnym aspektem generatora liczb losowych w Elm jest fakt, że jest on całkowicie deterministyczny. Oznacza to, że przy tym samym seedzie i funkcji generującej, zawsze otrzymamy dokładnie takie same wyniki. Jest to bardzo przydatne w testach jednostkowych, kiedy chcemy mieć pełną kontrolę nad wynikami testów.

## Zobacz także

- [Dokumentacja Elm Matematyka](https://package.elm-lang.org/packages/elm-explorations/math/1.0.0/)
- [Przykładowy projekt losujący karty w Elm](https://github.com/elm-lang/random-card-generator)
- [Wprowadzenie do języka Elm](https://www.freecodecamp.org/news/learning-functional-programming-with-elm/)