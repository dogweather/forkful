---
title:                "Konwertowanie ciągu na duże litery"
html_title:           "Elm: Konwertowanie ciągu na duże litery"
simple_title:         "Konwertowanie ciągu na duże litery"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Po co

Uwzględnianie wielkich liter w ciągu znaków oznacza przekształcenie wszystkich liter w nim na wielkie litery. Programiści często wykonują tę operację, gdy chcą porównać dwa ciągi znaków, ponieważ wielkość liter może mieć wpływ na wynik porównania.

## Jak to zrobić:

```Elm
String.toUpper "hej!" -- wynik: "HEJ!"
```

```Elm
String.toUpper "ELM" -- wynik: "ELM"
```

```Elm
String.toUpper "Bardzo1dziwny6ciąg67!@#" -- wynik: "BARDZO1DZIWNY6CIĄG67!@#"
```

## Głębszy wgląd

Uwzględnianie wielkich liter w ciągu znaków jest powszechnie stosowane w programowaniu i ma swoje korzenie w systemach operacyjnych, które zwykle traktują wielkość liter jako ważną przy porównywaniu nazw plików i folderów. Istnieją również inne sposoby na wykrywanie różnic w ciągach znaków, takie jak uwzględnienie przypadku lub wykluczenie znaków specjalnych. Jednak uwzględnianie wielkich liter jest szybkie i proste do zastosowania w przypadku prostych porównań.

## Zobacz także

Uwzględnianie wielkich liter ma zastosowanie w wielu innych językach programowania, takich jak Java, Python i JavaScript. Jeśli chcesz dowiedzieć się więcej na ten temat, możesz przeczytać dokumentację w wybranym języku lub poszukać poradników online.