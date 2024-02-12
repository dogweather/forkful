---
title:                "Tworzenie pliku tymczasowego"
aliases: - /pl/swift/creating-a-temporary-file.md
date:                  2024-01-20T17:41:17.482508-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tworzenie pliku tymczasowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku pozwala na przechowywanie danych tymczasowo podczas działania aplikacji. Programiści robią to, aby na przykład uniknąć zaśmiecania dysku stałymi plikami czy przeprowadzić operacje na danych, które są potrzebne tylko przez chwilę.

## Jak to zrobić:
```Swift
import Foundation

// Tworzenie unikalnej ścieżki do tymczasowego pliku
let tempDirectoryURL = FileManager.default.temporaryDirectory
let tempFileURL = tempDirectoryURL.appendingPathComponent("mojTymczasowyPlik.txt")

// Zapisywanie danych do tymczasowego pliku
let sampleText = "Przykładowy tekst"
do {
    try sampleText.write(to: tempFileURL, atomically: true, encoding: .utf8)
    print("Plik tymczasowy zapisany: \(tempFileURL.path)")
} catch {
    print("Wystąpił błąd: \(error)")
}

// Odczytywanie danych z tymczasowego pliku
do {
    let readText = try String(contentsOf: tempFileURL, encoding: .utf8)
    print("Zawartość pliku tymczasowego: \(readText)")
} catch {
    print("Wystąpił błąd podczas odczytu: \(error)")
}
```

Sample output:
```
Plik tymczasowy zapisany: /var/folders/.../T/mojTymczasowyPlik.txt
Zawartość pliku tymczasowego: Przykładowy tekst
```

## Deep Dive
Tymczasowe pliki były używane we wczesnych dniach informatyki, kiedy pamięć była ograniczona. Tworzenie takich plików odciąża pamięć RAM, przenosząc dane na dysk. W Swift, `FileManager` jest głównym narzędziem do zarządzania systemem plików, z metodą `temporaryDirectory` zwracającą URL do lokalizacji, w której można bezpiecznie zapisywać tymczasowe pliki. Alternatywą jest użycie `mkstemp()` z UNIX API dla większej kontroli, ale to zazwyczaj niepotrzebne w większości aplikacji Swift. Ważne jest, żeby pamiętać o usuwaniu tymczasowych plików, gdy nie są już potrzebne, choć system operacyjny często zrobi to za nas po zamknięciu aplikacji.

## Zobacz również
- [Apple Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Stack Overflow discussion on temporary files in Swift](https://stackoverflow.com/questions/38090923/how-to-create-a-temporary-file-in-swift)
