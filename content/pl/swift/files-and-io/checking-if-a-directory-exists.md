---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.097436-07:00
description: "Sprawdzanie, czy katalog istnieje w systemie plik\xF3w, jest niezb\u0119\
  dne do zarz\u0105dzania strukturami plik\xF3w w aplikacjach Swift. To zadanie umo\u017C\
  liwia\u2026"
lastmod: '2024-03-11T00:14:08.975792-06:00'
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje w systemie plik\xF3w, jest niezb\u0119\
  dne do zarz\u0105dzania strukturami plik\xF3w w aplikacjach Swift. To zadanie umo\u017C\
  liwia\u2026"
title: Sprawdzanie, czy katalog istnieje
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w systemie plików, jest niezbędne do zarządzania strukturami plików w aplikacjach Swift. To zadanie umożliwia programistom weryfikację obecności katalogów przed próbą odczytu z nich lub zapisu do nich, co pozwala uniknąć możliwych błędów w czasie wykonania.

## Jak to zrobić:

Framework Foundation w Swifcie dostarcza klasę `FileManager`, która posiada metody do zarządzania systemem plików. Możesz użyć `FileManager`, aby sprawdzić, czy katalog istnieje. Oto fragment kodu, jak to zrobić:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/ścieżka/do/twojego/katalogu"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Katalog istnieje")
} else {
    print("Katalog nie istnieje")
}
```

Jednak sprawdza to zarówno pliki, jak i katalogi. Jeśli chcesz specjalnie zweryfikować, czy katalog istnieje, musisz przekazać wskaźnik do wartości logicznej w `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/ścieżka/do/twojego/katalogu"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Katalog istnieje")
} else {
    print("Katalog nie istnieje")
}
```

### Używanie biblioteki zewnętrznej

Na obecną chwilę, sprawdzenie istnienia katalogu w Swifcie zazwyczaj nie wymaga stosowania bibliotek zewnętrznych z powodu solidności klasy `FileManager`. Jednak dla bardziej złożonych manipulacji i kontroli plików, biblioteki takie jak **Files** autorstwa Johna Sundella dostarczają bardziej przyjazne dla Swifta API.

Oto jak możesz jej użyć:

Najpierw dodaj Files do swojego projektu za pomocą Swift Package Manager.

Następnie, możesz sprawdzić istnienie katalogu w ten sposób:

```swift
import Files

do {
    _ = try Folder(path: "/ścieżka/do/twojego/katalogu")
    print("Katalog istnieje")
} catch {
    print("Katalog nie istnieje")
}
```

Uwaga: Ponieważ biblioteki zewnętrzne mogą ulegać zmianom, zawsze odwołuj się do najnowszej dokumentacji w celu uzyskania informacji o użytkowaniu i najlepszych praktykach.
