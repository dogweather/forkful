---
title:    "Swift: Sprawdzanie istnienia katalogu"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego?

Sprawdzanie istnienia katalogu jest kluczowym elementem programowania w języku Swift. Dzięki temu narzędziu można sprawdzić, czy dany katalog istnieje na naszym urządzeniu i podjąć odpowiednie działania w zależności od wyniku. Jest to szczególnie ważne w przypadku programów, które operują na dużej ilości plików i katalogów.

## Jak to zrobić?

Sprawdzenie istnienia katalogu w języku Swift jest bardzo proste. Wystarczy użyć metody `FileManager.default.fileExists(atPath:)`, podając jako argument ścieżkę do katalogu, który chcemy sprawdzić.

```Swift
if FileManager.default.fileExists(atPath: "/Users/UserName/Documents") {
    print("Katalog istnieje!")
} else {
    print("Katalog nie istnieje.")
}

// Output: Katalog istnieje!
```

Możemy także użyć metody `fileExists(atPath:)` na instancji obiektu `FileManager`, jeśli chcemy sprawdzić istnienie katalogu w konkretnym miejscu.

```Swift
let fileManager = FileManager()
if fileManager.fileExists(atPath: "/Users/UserName/Desktop") {
    print("Katalog istnieje!")
} else {
    print("Katalog nie istnieje.")
}

// Output: Katalog istnieje!
```

## Deep Dive

Ta metoda zwraca wartość logiczną (true lub false) w zależności od istnienia katalogu. Możemy także użyć metody `fileExists(at:)` na instancji obiektu `FileManager`, podając jako argument obiekt typu `URL` reprezentujący ścieżkę do katalogu. `FileManager` obsługuje także różne metody sprawdzania istnienia plików i katalogów w zależności od naszych potrzeb, na przykład `fileExists(atPath:)` tylko dla określonych atrybutów, jak rozmiar czy datę modyfikacji.

## Zobacz też

- [Dokumentacja Apple na temat FMKManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial na temat pracy z plikami i katalogami w języku Swift](https://www.raywenderlich.com/6015-working-with-files-in-swift)
- [Przydatny artykuł o wykorzystywaniu FileManager w Swift](https://www.hackingwithswift.com/read/10/2/reading-and-writing-basics-nsfilemanager)