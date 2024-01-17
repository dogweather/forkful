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

## Czym jest tworzenie tymczasowych plików i dlaczego programiści to robią?

Tworzenie tymczasowych plików jest to proces tworzenia i usuwania plików, które są przeznaczone tylko na chwilę. Programiści często tworzą tymczasowe pliki, gdy potrzebują tymczasowo przechować pewne dane lub gdy potrzebują tymczasowego miejsca do pracy nad kodem lub eksperymentów.

## Jak to zrobić?

### Przykład 1:
```Swift
let fileName = NSTemporaryDirectory().appending("test.txt") 
FileManager.default.createFile(atPath: fileName, contents: "Hello World".data(using: .utf8))
// Tworzy pusty plik o nazwie "test.txt" i zapisuje w nim string "Hello World".
```

Po wykonaniu tego kodu, w folderze tymczasowym zostanie utworzony plik "test.txt" z zawartością "Hello World".

### Przykład 2:
```Swift
do {
    let tempDir = NSTemporaryDirectory()
    let tempURL = URL(fileURLWithPath: tempDir).appendingPathComponent("image.jpg")
    let imageData = try Data(contentsOf: URL(string: "https://example.com/image.jpg")!)
    try imageData.write(to: tempURL)
} catch {
    print("Nie udało się utworzyć tymczasowego pliku.")
}
// Pobiera obraz z adresu URL, tworzy w folderze tymczasowym plik "image.jpg" i zapisuje w nim pobrane dane.
```

## Głębokie zanurzenie

### Kontekst historyczny:

Tworzenie tymczasowych plików jest powszechnie stosowaną praktyką w programowaniu od lat. Wcześniej, gdy pamięć komputera była ograniczona, tworzenie tymczasowych plików było szczególnie ważne, ponieważ pozwalało zachować zasoby pamięci i uniknąć błędów.

### Alternatywy:

Alternatywą dla tworzenia tymczasowych plików może być wykorzystanie innych sposobów przechowywania danych, takich jak zmienne czy tablice. Jednak w niektórych przypadkach tworzenie tymczasowych plików jest preferowane ze względu na jego prostotę i wygodę.

### Szczegóły implementacji:

W języku Swift, najprostszym sposobem utworzenia tymczasowego pliku jest użycie metody ```createFile(atPath:contents:attributes:)``` z klasy ```FileManager```. Ta metoda tworzy pusty plik w podanej ścieżce i nadaje mu określone atrybuty. Aby uzyskać dostęp do folderu tymczasowego, można wykorzystać metodę ```NSTemporaryDirectory()```, która zwraca ścieżkę do tego folderu.

## Zobacz też

- Dokumentacja Swift na temat tworzenia tymczasowych plików: https://developer.apple.com/documentation/foundation/filemanager/1415241-createfile
- Artykuł na temat wykorzystywania folderu tymczasowego w aplikacjach iOS: https://www.raywenderlich.com/832-apriltagesture-swift-tutorial-getting-started
- Przewodnik po tworzeniu i zarządzaniu plikami w języku Swift: https://www.hackingwithswift.com/read/10/0/introduction