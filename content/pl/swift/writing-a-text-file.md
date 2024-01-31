---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zapisywanie pliku tekstowego to proces, w którym dane są zapisywane w pliku o formacie tekstowym. Programiści robią to, aby przechowywać dane konfiguracyjne, logować informacje lub wyeksportować wyniki pracy programu.

## Jak to zrobić?

```Swift
import Foundation

let str = "Witaj świecie!"
let filename = getDocumentsDirectory().appendingPathComponent("output.txt")

do {
    try str.write(to: filename, atomically: true, encoding: String.Encoding.utf8)
    print("Plik został zapisany.")
} catch {
    print("Wystąpił błąd podczas zapisu pliku: \(error).")
}

// Pomocnicza funkcja do znalezienia katalogu dokumentów
func getDocumentsDirectory() -> URL {
    let paths = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)
    return paths[0]
}
```

**Przykładowy wynik:**
```
Plik został zapisany.
```

## Deep Dive

Zapisywanie plików tekstowych w Swift zaczęło się od wersji Swift 1.0 i ewoluowało na przestrzeni lat. Alternatywą może być użycie `FileHandle` czy strumieni danych (`OutputStream`). Dane w pliku tekstowym są zapisywane jako ciąg znaków w wybranym kodowaniu, najczęściej UTF-8.

## Zobacz też

- [Dokumentacja Apple o pracy z systemem plików](https://developer.apple.com/documentation/foundation/file_system)
- [Dokumentacja Apple o klasie `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial o obsłudze plików w Swift](https://www.hackingwithswift.com/read/26/overview)
