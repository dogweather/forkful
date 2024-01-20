---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Szukanie i zastępowanie tekstu to podstawowe operacje w pracy programisty, które pozwalają na szybkie znalezienie i zamianę określonych ciągów znaków w tekście. Jest niezbędne w wielu dziedzinach programowania, takich jak analiza danych czy modyfikacja plików.

## Jak zrobić:
Żeby wyszukać i zastąpić tekst w Elm, wykorzystamy funkcję `String.replace`. Zobacz poniższy przykład:

```Elm
String.replace "pies" "kot" "Mam piesa i dwa koty"
```

Tekst "Mam piesa i dwa koty" zostanie zmieniony na "Mam kota i dwa koty". Proste, prawda?

## Pogłębiona analiza
Szukanie i zamiana tekstu jest stara jak pisarstwo. Najpierw było używane w edytorach tekstu, a później zaimplementowano je w różnych językach programowania. 
Alternatywą dla `String.replace` w Elm jest używanie funkcji `List.map`, która pozwala na przechodzenie przez każdy element listy i wykonanie na nim określonej funkcji.

```Elm
List.map (String.replace "pies" "kot") ["Mam piesa", "i dwa koty"]
```
W ten sposób również zastąpisz "pies" "kotem", ale w liście stringów zamiast pojedynczego stringa.

## Zobacz również
Gorąco zachęcam do odwiedzenia oficjalnej dokumentacji Elm, gdzie znajdziesz więcej informacji na temat funkcji `String.replace`: https://package.elm-lang.org/packages/elm/core/latest/String#replace. Spójrz również na moduł `List`, jeśli jesteś zainteresowany alternatywą: https://package.elm-lang.org/packages/elm/core/latest/List#map.