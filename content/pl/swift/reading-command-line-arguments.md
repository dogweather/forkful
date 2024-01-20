---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów linii poleceń to zrozumienie danych przekazanych do programu podczas jego uruchomienia. Programiści robią to, żeby kontrolować i dostosowywać działanie aplikacji z poziomu terminala.

## Jak to zrobić:

```Swift
let arg = CommandLine.arguments[1]
print(arg)
```

Ten kod wczytuje pierwszy argument przekazany do programu i wyświetla go w konsoli. Na przykład:

```Shell
$ swift app.swift argument
```

Na ekranie zobaczysz:

```
argument
```

## W głąb tematu

Historia: Argumenty linii poleceń to standardowa metoda interakcji z programem, wszechobecna od początku historii Unixa.

Alternatywy: Możesz zastosować pliki konfiguracyjne, interfejsy graficzne użytkownika lub interaktywne konsolowe menu do przekazywania danych do programu. Wybór zależy od wymagań aplikacji i preferencji użytkownika.

Szczegóły implementacji: Swift traktuje argumenty linii poleceń jako tablicę łańcuchów znaków, gdzie pierwszy element (`CommandLine.arguments[0]`) to nazwa programu, a każdy kolejny to argumenty przekazane do programu.

## Zobacz także

Dokumentacja Apple o argumentach linii poleceń: (https://developer.apple.com/documentation/swift/commandline)

Arthur Ariel Sabintsev na temat wczytywania argumentów linii poleceń w Swift: (https://www.swiftbysundell.com/articles/command-line-arguments-in-swift-script/)