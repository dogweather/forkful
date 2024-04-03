---
date: 2024-01-20 17:47:11.206096-07:00
description: "How to: W Elm u\u017Cywa si\u0119 funkcji `String.length` do uzyskania\
  \ d\u0142ugo\u015Bci ci\u0105gu znak\xF3w. Oto przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.312281-06:00'
model: gpt-4-1106-preview
summary: "W Elm u\u017Cywa si\u0119 funkcji `String.length` do uzyskania d\u0142ugo\u015B\
  ci ci\u0105gu znak\xF3w."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
