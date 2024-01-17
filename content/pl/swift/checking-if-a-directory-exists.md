---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Swift: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Sprawdzanie, czy istnieje katalog, jest procesem, który pozwala programistom na potwierdzenie istnienia danego katalogu na komputerze. Jest to ważny krok w wielu aplikacjach, ponieważ pozwala na sprawdzenie, czy potrzebny katalog jest dostępny przed wykonaniem operacji na plikach wewnątrz niego.

## Jak to zrobić:
```Swift
if FileManager.default.fileExists(atPath: "/Users/UserName/Desktop") { //dostosuj ścieżkę katalogu do swoich potrzeb
    print("Katalog istnieje!")
} else {
    print("Katalog nie istnieje.")
}
```
Wyjście:
```
Katalog istnieje!
```

## Głębszy przegląd:
Sprawdzanie, czy istnieje katalog, jest często stosowane w aplikacjach do zarządzania plikami, na przykład w edytorach tekstu czy programach do przetwarzania obrazów. Innym sposobem na sprawdzenie istnienia katalogu jest użycie metody ```fileExists``` w klasie ```NSFileManager```. W nowych wersjach Swift używane jest po prostu ```FileManager``` zamiast ```NSFileManager```.

## Zobacz także:
[Oficjalna dokumentacja Swift](https://docs.swift.org/swift-book/LanguageGuide/ClassesAndStructures.html#//apple_ref/doc/uid/TP40014097-CH13-ID82) - dostęp do wersji języka Swift w którym ogłoszono zmiany w klasach i strukturach.
[NSFileManager w iOS](https://developer.apple.com/documentation/foundation/nsfilemanager) - dokumentacja Apple dla klas NSFileManager.