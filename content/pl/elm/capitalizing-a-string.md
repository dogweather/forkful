---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Elm: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zamiana stringa na kapitałki oznacza przekształcenie wszystkich małych liter na duże litery. Programiści często korzystają z tego podczas monitorowania wprowadzania tekstu, aby zapewnić spójność i dokładność danych.

## Jak to zrobić:

Elm, w przeciwieństwie do niektórych języków programowania, nie ma wbudowanej funkcji konwertującej stringi na większe litery. Musisz wykorzystać 'toUpper' z modułu 'Char'. Poniżej przykład kodu:

```Elm
import Char

capitalize : String -> String
capitalize string =
    let
        toUpperString = 
            String.foldl (\char acc -> Char.toUpper char :: acc) [] string
    in
    String.fromList (List.reverse toUpperString)
```
Sample output:

```Elm
capitalize "hello"
-- Output: "HELLO"
```

## Podróż w głąb wiedzy:

Historia konwersji stringów na litery duże sięga wczesnej historii informatyki, gdzie monitory były wyposażone tylko w wyświetlanie dużych liter.

Są różne metody i funkcje, które można zastosować do tego celu. Niektóre języki programowania, takie jak JavaScript, mają wbudowane funkcje realizujące to zadanie.

Elm, mimo braku wbudowanej metody, pozwala skorzystać z funkcji 'toUpper' z modułu 'Char', dzięki czemu możemy łatwo zbudować naszą własną funkcję do tego celu. Technika ta jest bardziej elastyczna, ponieważ pozwala na kontrolę szczegółów implementacji.

## Zobacz również:

W celu rozszerzenia wiedzy o tym temacie, oto kilka przydatnych źródeł:

- Dokumentacja Elm - https://package.elm-lang.org/packages/elm/core/latest/String- Dostarcza informacji na temat wbudowanych funkcji operujących na stringach w Elm.

- Publikacja na blogu o przetwarzaniu stringów w Elm - https://korban.net/posts/elm/2019-11-18-practical-guide-string-processing-techniques-elm/ - Praktyczny przewodnik po technikach przetwarzania stringów w Elm.

- Dokumentacja Elm Char toUpper - https://package.elm-lang.org/packages/elm/core/latest/Char#toUpper - Dokumentacja na temat funkcji 'toUpper' modułu 'Char'.