---
date: 2024-01-20 17:32:42.576260-07:00
description: "How to: (Jak to zrobi\u0107:) Historia Elm i obs\u0142ugi czasu w j\u0119\
  zykach funkcyjnych ma swoje korzenie w Haskellu, z kt\xF3rego funkcjonalno\u015B\
  ci i typowo\u015Bci czerpie\u2026"
lastmod: '2024-04-05T22:50:49.649411-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historia Elm i obs\u0142ugi czasu w j\u0119zykach\
  \ funkcyjnych ma swoje korzenie w Haskellu, z kt\xF3rego funkcjonalno\u015Bci i\
  \ typowo\u015Bci czerpie Elm."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## How to: (Jak to zrobić:)
```Elm
import Time exposing (Posix)
import Time.Extra exposing (compare)

dateComparison : Posix -> Posix -> Order
dateComparison date1 date2 =
    compare date1 date2

-- Przykład użycia:
date1 : Posix
date1 =
    Time.millisToPosix 1588291200000

date2 : Posix
date2 =
    Time.millisToPosix 1588377600000

-- Sprawdzamy, która data jest wcześniejsza
result : Order
result =
    dateComparison date1 date2

-- Wyświetl wynik:
-- LT oznacza, że date1 jest wcześniejsza niż date2
-- EQ oznacza, że daty są równe
-- GT oznacza, że date1 jest późniejsza niż date2
```

## Deep Dive (Dogłębna analiza):
Historia Elm i obsługi czasu w językach funkcyjnych ma swoje korzenie w Haskellu, z którego funkcjonalności i typowości czerpie Elm. Alternatywnie, można porównywać daty przekształcając je na liczby typu `Int` (timestamp), ale traci się informacje o strefach czasowych. Kiedy używamy `Time.Extra.compare`, Elm porównuje wartości `Posix` uwzględniając te czynniki, zapewniając precyzyjne i unikatowe wyniki porównań.

## See Also (Zobacz również):
- Dokumentacja Elm Time: https://package.elm-lang.org/packages/elm/time/latest/
- Pakiet Time.Extra: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Poradnik Elm'a o czasie: https://guide.elm-lang.org/effects/time.html
