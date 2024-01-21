---
title:                "Wyszukiwanie i zamiana tekstu"
date:                  2024-01-20T17:57:44.333418-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i dlaczego?"

Szukanie i zamiana tekstu to podstawowe operacje pozwalające na lokalizowanie wzorców w tekście oraz ich modyfikację. Programiści używają tej techniki do refaktoryzacji, poprawiania błędów, czy też zmian masowych w kodzie.

## How to:
"Jak to zrobić:"

```Elm
import String

-- Funkcja szukająca i zastępująca tekst w Elm
replaceText : String -> String -> String -> String
replaceText find replace text =
    String.split find text
        |> String.join replace

-- Przykład użycia
example : String
example =
    replaceText "hello" "hi" "hello there, hello world!"

-- Wynik: "hi there, hi world!"
```

## Deep Dive
"Dogłębna analiza"

Szukanie i zamiana tekstu to jedne z najstarszych pojęć w informatyce, sięgające czasów edytorów tekstów i początków programowania. W elm każda manipulacja tekstem odbywa się przez funkcje w module `String`, co świadczy o jej bezstanowości i funkcyjnym charakterze. Alternatywy? W innych językach, jak JavaScript, zastosujesz wyrażenia regularne, które są potężne, ale skomplikowane. Elm trzyma rzeczy proste, oferując czyste, czytelne API bez wirów wyrażeń regularnych. To znaczy, że operacje na tekstach są mniej elastyczne, ale bardziej przewidywalne i łatwe w utrzymaniu.

## See Also
"Zobacz także"

- Elm `String` documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)