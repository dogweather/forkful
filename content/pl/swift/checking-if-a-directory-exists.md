---
title:                "Swift: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie istnienia katalogu może być ważnym elementem w procesie programowania aplikacji. Pozwala to na dynamiczne dostosowywanie działań w zależności od obecności lub braku określonego katalogu. Jest to szczególnie przydatne, gdy aplikacja ma dużo plików i katalogów, a programista chce zapewnić, że wszystko działa poprawnie.

## Jak to zrobić

Aby sprawdzić czy katalog istnieje, można skorzystać z metody `FileManager.default.fileExists(atPath: )`. Poniżej znajduje się przykładowy kod, który pokazuje jak można wykorzystać tę metodę:

```Swift
let fileManager = FileManager.default
let path = "/Users/John/Documents/Projects"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Katalog istnieje.")
    } else {
        print("To nie jest katalog.")
    }
} else {
    print("Katalog nie istnieje.")
}
```

Powyższy kod sprawdzi czy w podanym ścieżce znajduje się istniejący katalog i w zależności od tego wyświetli odpowiedni komunikat. Warto również zwrócić uwagę na wykorzystanie zmiennej `isDirectory` typu `ObjCBool`, która pozwala na rozróżnienie między plikiem a katalogiem.

## Wnikliwa analiza

Metoda `fileExists(atPath: )` wykorzystuje podstawową klasę `FileManager`, która jest odpowiedzialna za zarządzanie plikami i katalogami w systemie. W przypadku sprawdzania istnienia katalogu, metoda ta przekazuje informacje o tym, czy w podanej ścieżce znajduje się katalog czy nie, poprzez argument `isDirectory` typu `ObjCBool`. Dodatkowo, jeśli jest to katalog, można użyć innych metod z tej klasy do dalszego wykonywania operacji na nim.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wykorzystywaniu klas i metod w języku Swift, zapoznaj się z następującymi linkami:

- [Dokumentacja oficjalna języka Swift](https://docs.swift.org/swift-book/)
- [Udemy - Szybkie wprowadzenie do programowania w języku Swift](https://www.udemy.com/course/szybkie-wprowadzenie-do-programowania-w-jezyku-swift/)
- [Książka "Swift 5 - zbiór przepisów"](https://helion.pl/ksiazki/swift-5-zbior-przepisow-poznaj-tajniki-jezyka-swift-marcin-krzyzanowski,dswfif.htm)