---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana łańcucha znaków (string) na małe litery to proces polegający na zmianie wszystkich dużych liter w łańcuchu znaków na ich małe odpowiedniki. Programiści robią to, aby zapewnić jednolitość danych i uniknąć problemów związków z wielkością liter.

## Jak to zrobić:

Jest to proste do osiągnięcia w Elm. Poniżej znajduje się kod, który konwertuje dane wejściowe na małe litery.

```Elm
import String

lowerCaseString : String -> String
lowerCaseString str = String.toLower str

main =
    let
        input = "Hello, World!"
    in
    lowerCaseString input
```

Przypadki testowe:

```Elm
-- "Hello, World!" -> "hello, world!"
lowerCaseString "Hello, World!"
-- "POlIsH tEXt" -> "polish text"
lowerCaseString "POlIsH tEXt"
```

## W głąb tematu:

Zmiana łańcucha znaków na małe litery to często używany proces w różnych językach programowania, nie tylko w Elm. Przenośność pomiędzy różnymi językami jest w zasadzie identyczna. W Elm, funkcja `toLower` z biblioteki `String` jest wykorzystywana do tego zadania.

Alternatywą dla `String.toLower` jest napisanie własnej funkcji, która przejdzie przez każdą literę i zmieni ją na małą, ale to pociąga za sobą dużo więcej pracy i zwiększa złożoność kodu.

Co do szczegółów implementacji, `String.toLower` w Elm jest funkcją, która korzysta z natywnego API przeglądarki do konwersji. W szczególności, korzysta ona z metody JavaScript `toLowerCase()`, która jest standardem przemysłowym.

## Zobacz również:

1. [Dokumentacja Elm String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
2. [JavaScript toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) - Bezpośrednia metoda JavaScript, na której bazuje `String.toLower` w Elm.