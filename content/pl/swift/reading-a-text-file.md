---
title:    "Swift: Odczytywanie pliku tekstowego"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś lub potrzebowałeś przeczytać plik tekstowy w swoim programie Swift? Może chcesz przetworzyć dane lub wyświetlić zawartość pliku użytkownikowi. W tym artykule dowiesz się, jak możesz łatwo odczytać plik tekstowy przy użyciu języka Swift.

## Jak

```Swift
// Utwórz ścieżkę do pliku tekstowego
let filePath = "sciezka/do/pliku.txt"

// Użyj metody String do odczytania zawartości pliku jako ciągu tekstowego
if let fileContents = try? String(contentsOfFile: filePath) {
    print(fileContents)
} else {
    // W przypadku błędu wyświetl komunikat
    print("Nie można odczytać pliku.")
}
```

Wykorzystując powyższy kod, możesz łatwo przeczytać zawartość pliku tekstowego i wyświetlić ją na konsoli. Istnieją jednak również inne metody odczytu plików, takie jak wykorzystanie enumeratora lub wykorzystanie biblioteki Foundation.

## Deep Dive

Odczytywanie plików tekstowych może być przydatne w różnych sytuacjach, na przykład podczas analizowania danych lub tworzenia interfejsu użytkownika z dostępnymi opcjami. Istnieje wiele metod obsługi odczytu plików tekstowych, dlatego warto eksperymentować i znaleźć najlepszą dla swojego projektu.

## Zobacz także

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial odczytu i zapisu plików w Swift](https://www.ralfebert.de/ios/tutorials/filemanager/)
- [Przykładowy projekt czytający plik tekstowy w Swift](https://github.com/sunderijan/swift-reading-text-file/blob/master/README.md)