---
title:                "Pisanie do standardowego błędu"
html_title:           "Elm: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego wyjścia błędu jest niezbędną częścią programowania w Elm. Pozwala nam szybko i efektywnie diagnozować problemy i błędy w naszych aplikacjach. W tym artykule omówimy podstawowe informacje na temat pisania do standardowego wyjścia błędu w Elm oraz pokażemy przykładowe kody.

## Jak to zrobić

Możemy napisać do standardowego wyjścia błędu w Elm za pomocą funkcji `Debug.crash` lub `Debug.fatal`. W obu przypadkach podajemy komunikat, który chcemy wyświetlić w konsoli. Oto przykład użycia `Debug.crash`:

```Elm
import Debug exposing (crash)

age : Int
age = 25

main =
  if age < 18 then
    "Jesteś niepełnoletni!"
  else if age > 18 && age < 65 then
    "Jesteś dorosły!"
  else
    crash "Jesteś emerytem!"
```
Wynik powyższego kodu będzie wyglądać tak:

```
crash "Jesteś emerytem!"
^Jesteś emerytem!
```

## Wchodzi głębiej

Możemy także dostosować wyświetlane informacje dodając do wywołania funkcji `Debug.crash` nazwę funkcji, która zawiera błąd oraz informację debugową. Przykładowo:

```Elm
import Debug exposing (crash)

age : Int
age = 25

calculatePercentage : Float -> Float -> Float
calculatePercentage numerator denominator =
  if denominator == 0 then
    crash "calculatePercentage" "Denominator cannot be 0."
  else
    (numerator / denominator) * 100

main =
  calculatePercentage 50 0
```

Wynik:

```
calculatePercentage "Denominator cannot be 0."
^calculatePercentage "Denominator cannot be 0."
```

## Zobacz także

Dla dalszego zgłębienia tematu pisania do standardowego wyjścia błędu w Elm, polecamy zapoznanie się z oficjalną dokumentacją: 

- https://package.elm-lang.org/packages/elm/core/latest/Debug#crash
- https://package.elm-lang.org/packages/elm/core/latest/Debug#fatal