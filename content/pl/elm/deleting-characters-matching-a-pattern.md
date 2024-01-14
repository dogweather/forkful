---
title:                "Elm: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Może się zdarzyć, że w trakcie kodowania będziemy musieli usunąć znaki pasujące do określonego wzorca. W tym wpisie opowiemy o tym, jak zrobić to w języku Elm.

## Jak to zrobić

Załóżmy, że mamy string "aaa123bbb" i chcemy usunąć wszystkie cyfry z tego ciągu znaków. W Elm możemy użyć funkcji `String.filter` połączonej z funkcją `Char.isDigit` w następujący sposób:

```Elm
import String exposing (filter)
import Char exposing (isDigit)

string = "aaa123bbb"
result = string |> filter (\c -> not (isDigit c))
```

W powyższym kodzie używamy operatora `|>` do przesyłania stringa przez funkcję `filter`. Wewnątrz `filter` używamy lambdy, która sprawdza czy znak nie jest cyfrą i jeśli tak, to zostaje on usunięty. Wynikiem będzie string "aaabbb".

Aby bardziej zrozumieć działanie funkcji `String.filter`, poniżej przedstawiamy kolejne kroki w ich wykonaniu:

1. Tworzymy string do przefiltrowania (w tym przypadku "aaa123bbb").
2. Tworzymy funkcję lambdę, która przyjmuje pojedynczy znak i zwraca `True` lub `False` w zależności od tego, czy dany znak pasuje do wzorca do usunięcia (w tym przypadku cyfry).
3. Przepuszczamy nasz string przez funkcję `filter`, która usunie wszystkie znaki, dla których funkcja lambda zwróciła `True`.
4. Wynikiem jest nowy string bez znaków pasujących do wzorca.

Tak samo można usunąć inne rodzaje znaków, np. spacje, przecinki czy znaki specjalne. Wystarczy odpowiednio dostosować funkcję `Char.isDigit` do naszych potrzeb.

## Deep Dive

Funkcja `String.filter` jest często używana do operowania na stringach w języku Elm. Jest nie tylko użyteczna do usuwania znaków pasujących do wzorca, ale także do filtrowania stringów na podstawie innych kryteriów, np. długości czy występowania określonych słów.

Warto również wspomnieć o funkcji `String.explode`, która dzieli string na listę pojedynczych znaków. Może być przydatna w przypadku, gdy chcemy wykonać bardziej skomplikowane operacje na poszczególnych znakach i ponownie połączyć je w stringu końcowym.

## Zobacz także

- [Dokumentacja funkcji String.filter](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Przykłady zastosowania funkcji String.filter](https://trenning.ghost.io/elm-string-filter/)
- [Dokumentacja funkcji String.explode](https://package.elm-lang.org/packages/elm-lang/core/latest/Basics#String-explode)