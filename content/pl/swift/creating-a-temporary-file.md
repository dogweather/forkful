---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Swift: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest częstym zadaniem w programowaniu, szczególnie gdy pracujemy z dużą ilością danych lub potrzebujemy czasowo przechować pewne informacje. Poniżej przedstawiamy sposoby na stworzenie tymczasowego pliku w języku Swift.

## Jak to zrobić

```Swift
// Tworzenie pliku tymczasowego o nazwie "temp.txt"
let temporaryFile = NSTemporaryDirectory().appendingPathComponent("temp.txt")

do {
    // Utworzenie tymczasowego pliku
    try FileManager.default.createFile(atPath: temporaryFile, contents: nil, attributes: nil)
} catch {
    // Obsługa błędu, jeśli nie udało się stworzyć pliku
    print("Nie można utworzyć pliku tymczasowego.")
}

// Wypisanie ścieżki do stworzonego pliku
print(temporaryFile)

// Usunięcie pliku tymczasowego
do {
    try FileManager.default.removeItem(atPath: temporaryFile)
} catch {
    print("Nie można usunąć pliku tymczasowego.")
}
```

**Wynik:**

*/var/folders/37/z95jk2kx4mz75y_d48x_ls8r0000gn/T/temp.txt*

## Głębsze spojrzenie

Podczas tworzenia pliku tymczasowego ważne jest, aby pamiętać, że jest to tylko tymczasowe rozwiązanie. Pliki tymczasowe są automatycznie usuwane po zakończeniu działania programu, więc nie są odpowiednie do przechowywania ważnych danych.

Dodatkowo, można też wykorzystać klasy NSFileHandle oraz NSOutputStream do manipulacji danymi wewnątrz pliku tymczasowego.

## Zobacz także

- [The Swift Programming Language (Swift 5.0)](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [NSFileManager | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [Working with Files and Directories | Hacking with Swift](https://www.hackingwithswift.com/articles/118/working-with-files-and-directories-in-swift)