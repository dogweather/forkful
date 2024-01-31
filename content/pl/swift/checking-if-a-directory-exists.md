---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:58:54.072154-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Sprawdzanie istnienia katalogu to sposób na upewnienie się, że folder faktycznie znajduje się na dysku. Programiści robią to, by zapobiec błędom podczas próby dostępu do nieistniejących ścieżek lub zapisu danych.

## How to:
W Swift możesz użyć `FileManager` do sprawdzenia istnienia katalogu. Oto przykład:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Katalog istnieje.")
} else {
    print("Katalog nie istnieje.")
}
```

Wyjście zależy od istnienia katalogu:
```
Katalog istnieje.
```
lub
```
Katalog nie istnieje.
```

Możesz też podać wskaźnik Bool jako drugi parametr, aby sprawdzić, czy ścieżka jest katalogiem:

```Swift
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("To jest katalog.")
    } else {
        print("To nie jest katalog, tylko plik.")
    }
}
```

## Deep Dive
Sprawdzanie istnienia katalogu sięga początków systemów plików. W Unix-owych systemach, na których wzoruje się iOS i macOS, istnieje pojęcie katalogów i plików, które są integralną częścią systemu plików.

We wcześniejszych wersjach Swift i Objective-C, `NSFileManager` pełnił podobną rolę co obecne `FileManager`. Oprócz prostego sprawdzania istnienia, `FileManager` oferuje metody do bardziej złożonych zadań związanych z systemem plików, jak tworzenie i usuwanie katalogów, listowanie zawartości czy zmiana uprawnień.

Możliwe alternatywy to bezpośrednie używanie API systemu operacyjnego, np. funkcji `stat` z libc w C, ale `FileManager` jest zdecydowanie bardziej "swiftowy" i bezpieczniejszy w obsłudze.

## See Also
- Dokumentacja Apple dotycząca `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Przykład używania `FileManager` w praktyce: https://www.hackingwithswift.com/example-code/system/how-to-find-the-path-to-a-file-in-your-bundle
- Obsługa błędów w Swift związana z plikami i katalogami: https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html
