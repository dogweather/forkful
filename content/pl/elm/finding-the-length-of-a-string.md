---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:11.206096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Długość ciągu znaków – to ile znaków zawiera. Programiści muszą to wiedzieć, aby np. weryfikować dane wejściowe czy obsługiwać teksty o zmiennej długości.

## How to:
W Elm używa się funkcji `String.length` do uzyskania długości ciągu znaków. Oto przykład:

```Elm
import String

main =
    let
        myString = "Cześć, świecie!"
    in
    String.length myString
    -- Wynik: 16
```

## Deep Dive
Historia języka Elm sięga 2012 roku. W Elm, operacja znalezienia długości ciągu jest prostsza niż w wielu innych językach – `String.length` robi to za nas. Alternatywy to własne funkcje rekurencyjne albo użycie bibliotek. Elm przechowuje stringi w UTF-16, więc `String.length` zwraca liczbę tzw. code units, co może być mylące przy specjalnych znakach czy emoji.

## See Also
Dalsze informacje i przykłady znajdziesz w [dokumentacji Elm](https://package.elm-lang.org/packages/elm/core/latest/String#length) oraz na stronie [Elm Guide](https://guide.elm-lang.org/).
