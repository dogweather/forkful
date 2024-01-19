---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Lua: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzenie, czy katalog istnieje, to proces, w którym program sprawdza, czy dany katalog jest obecny w systemie plików. Programiści wykonują to w celu zapobieżenia błędom związanym z próbą dostępu do katalogu, który nie istnieje.

## Jak to zrobić:

Korzystając z Swifta, możemy użyć FileManager klasy do sprawdzenia, czy katalog istnieje. Oto przykład:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/ścieżka/do/katalogu"

if fileManager.fileExists(atPath: path) {
    print("Katalog istnieje")
} else {
    print("Katalog nie istnieje")
}
```

Jeśli katalog istnieje, program wydrukuje "Katalog istnieje". W przeciwnym razie, otrzymasz "Katalog nie istnieje". 

## Głębszy wgląd

1. Kontekst historyczny: Sprawdzanie, czy katalog istnieje, to stara praktyka, która istnieje od początków programowania komputerowego. Było to niezbędne, aby zapewnić poprawne funkcjonowanie programów, które zależą od określonych struktur katalogów.
 
2. Alternatywy: W niektórych przypadkach, zamiast sprawdzać, czy katalog istnieje, programiści mogą po prostu próbować utworzyć katalog. Jeżeli katalog już istnieje, operacja jest ignorowana.

```Swift
do {
    try fileManager.createDirectory(atPath: path, withIntermediateDirectories: false, attributes: nil)
} catch {
    print("Błąd: \(error)")
}
```

3. Szczegóły implementacji: W Swiftie, metoda `fileExists(atPath:)` zwraca `true` jeśli katalog lub plik istnieje pod podaną ścieżką. Wykorzystuje ona API systemu plików, aby sprawdzić obecność katalogu.

## Zobacz też

- Apple Developer Documentation: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Stack Overflow: [How do I check if a directory exists in Swift?](https://stackoverflow.com/questions/24097826/read-and-write-a-string-from-text-file)
- Ray Wenderlich: [Working With Files & Directories in Swift](https://www.raywenderlich.com/6664-working-with-files-and-directories-in-swift)