---
date: 2024-01-20 17:47:11.206096-07:00
description: "D\u0142ugo\u015B\u0107 ci\u0105gu znak\xF3w \u2013 to ile znak\xF3w\
  \ zawiera. Programi\u015Bci musz\u0105 to wiedzie\u0107, aby np. weryfikowa\u0107\
  \ dane wej\u015Bciowe czy obs\u0142ugiwa\u0107 teksty o zmiennej d\u0142ugo\u015B\
  ci."
lastmod: '2024-02-25T18:49:33.676726-07:00'
model: gpt-4-1106-preview
summary: "D\u0142ugo\u015B\u0107 ci\u0105gu znak\xF3w \u2013 to ile znak\xF3w zawiera.\
  \ Programi\u015Bci musz\u0105 to wiedzie\u0107, aby np. weryfikowa\u0107 dane wej\u015B\
  ciowe czy obs\u0142ugiwa\u0107 teksty o zmiennej d\u0142ugo\u015Bci."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
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
