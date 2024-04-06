---
date: 2024-01-20 17:41:17.482508-07:00
description: "Jak to zrobi\u0107: Tymczasowe pliki by\u0142y u\u017Cywane we wczesnych\
  \ dniach informatyki, kiedy pami\u0119\u0107 by\u0142a ograniczona. Tworzenie takich\
  \ plik\xF3w odci\u0105\u017Ca pami\u0119\u0107 RAM,\u2026"
lastmod: '2024-04-05T22:50:50.113029-06:00'
model: gpt-4-1106-preview
summary: "Tymczasowe pliki by\u0142y u\u017Cywane we wczesnych dniach informatyki,\
  \ kiedy pami\u0119\u0107 by\u0142a ograniczona."
title: Tworzenie pliku tymczasowego
weight: 21
---

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
