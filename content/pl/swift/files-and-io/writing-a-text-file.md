---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:57.814832-07:00
description: "Jak to zrobi\u0107: Biblioteka standardowa Swifta zawiera wszystkie\
  \ narz\u0119dzia potrzebne do pisania plik\xF3w tekstowych. Oto podstawowe podej\u015B\
  cie."
lastmod: '2024-03-13T22:44:35.772990-06:00'
model: gpt-4-0125-preview
summary: "Biblioteka standardowa Swifta zawiera wszystkie narz\u0119dzia potrzebne\
  \ do pisania plik\xF3w tekstowych."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:


### Korzystając z Biblioteki Standardowej Swifta
Biblioteka standardowa Swifta zawiera wszystkie narzędzia potrzebne do pisania plików tekstowych. Oto podstawowe podejście:

```swift
import Foundation

let tresc = "Witajcie, czytelnicy Wired! Uczenie się Swifta jest zabawne."
let sciezkaPliku = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let nazwaPliku = "\(sciezkaPliku)/przyklad.txt"

do {
    try tresc.write(toFile: nazwaPliku, atomically: false, encoding: String.Encoding.utf8)
    print("Plik został pomyślnie zapisany")
} catch let error as NSError {
    print("Błąd zapisu do URL: \(nazwaPliku), Błąd: " + error.localizedDescription)
}
```

Ten fragment kodu zapisuje ciąg znaków do pliku o nazwie `przyklad.txt` w katalogu dokumentów. Obsługuje potencjalne błędy za pomocą mechanizmu obsługi błędów do-try-catch Swifta.

### Korzystając z FileManagera dla Większej Kontroli
Dla większej kontroli nad atrybutami plików lub aby sprawdzić, czy plik już istnieje, można użyć `FileManagera`:

```swift
import Foundation

let fileManager = FileManager.default
let katalogi = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let katalogDokumentow = katalogi.first {
    let URLPliku = katalogDokumentow.appendingPathComponent("przyklad.txt")
    let tresc = "Eksploracja Swifta do zarządzania plikami jest pouczająca."

    if fileManager.fileExists(atPath: URLPliku.path) {
        print("Plik już istnieje")
    } else {
        do {
            try tresc.write(to: URLPliku, atomically: true, encoding: .utf8)
            print("Plik został utworzony i pomyślnie zapisany")
        } catch {
            print("Błąd zapisu pliku: \(error)")
        }
    }
}
```

### Korzystając z Bibliotek Trzecich
Jedną z popularnych bibliotek trzecich do operacji na systemie plików w Swift jest `Files` autorstwa Johna Sundella:

Najpierw dodaj Files do swojego projektu, zazwyczaj za pomocą Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let pakiet = Package(
    name: "NazwaTwojegoPakietu",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "NazwaTwojegoCel",
            dependencies: ["Files"]),
    ]
)
```

Następnie użyj jej do zapisu do pliku:

```swift
import Files

do {
    let plik = try File(path: "/sciezka/do/twojego/katalogu/przyklad.txt")
    try plik.write(string: "Swift i biblioteka Files stanowią potężne połączenie.")
    print("Plik został pomyślnie zapisany przy użyciu biblioteki Files.")
} catch {
    print("Wystąpił błąd: \(error)")
}
```

Dzięki bibliotece `Files`, obsługa plików staje się bardziej bezpośrednia, co pozwala skupić się na logice biznesowej aplikacji, a nie na zawiłościach zarządzania plikami.
