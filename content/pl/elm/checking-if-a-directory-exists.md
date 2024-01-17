---
title:                "Sprawdzenie czy istnieje katalog"
html_title:           "Elm: Sprawdzenie czy istnieje katalog"
simple_title:         "Sprawdzenie czy istnieje katalog"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Po co?

Sprawdzanie czy dany katalog istnieje jest ważną częścią programowania w Elm. Dzięki temu, programiści mogą zabezpieczyć swoje aplikacje przed błędami i zapewnić lepszą kontrolę nad swoim kodem.

## Jak to zrobić:

Sprawdzenie czy katalog istnieje w Elm jest proste i może być wykonane za pomocą jednej z dostępnych funkcji. Oto przykładowy sposób:

```Elm
import File
import Maybe

directoryExists : String -> Bool
directoryExists path =
    Maybe.withDefault False (File.exists path)
```

Powyższy kod importuje moduł File, który zawiera funkcję `exists` służącą do sprawdzania istnienia plików i katalogów. Następnie definiuje funkcję `directoryExists`, która przyjmuje ścieżkę do katalogu jako argument i zwraca wartość True lub False w zależności od wyniku funkcji `exists`. 

Przykład użycia:

```Elm
directoryExists "/Users/username/Documents" --> True
directoryExists "/Users/username/Music" --> False
```

## Wnikliwe spojrzenie:

Sprawdzanie istnienia katalogów w Elm jest możliwe dzięki systemowi plików operacyjnego. Wiele języków programowania oferuje podobne funkcje, ale Elm wyróżnia się prostotą i czytelnością swojego kodu. Alternatywą dla funkcji `exists` może być wykorzystanie funkcji `listDirectory`, która zwraca listę plików i katalogów w danym katalogu.

## Zobacz także:

- Dokumentacja Elm: https://elm-lang.org/docs
- Moduł File w bibliotece standardowej Elm: https://package.elm-lang.org/packages/elm/file/latest/