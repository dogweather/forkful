---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Swift: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego
Sprawdzanie, czy katalog istnieje, jest ważną umiejętnością w programowaniu w Swift. Pozwala to na zapewnienie, że nasz kod działa poprawnie i świadczy o tym, że jesteśmy odpowiedzialnymi programistami.

## Jak to zrobić
W Swift, aby sprawdzić, czy katalog istnieje, możemy użyć klasy FileManager i jej metody fileExists(atPath). Przyjmie ona ścieżkę do naszego katalogu jako argument, a następnie zwróci wartość logiczną true lub false, w zależności od tego, czy katalog istnieje.

```Swift
let fileManager = FileManager.default
let path = "/ścieżka/do/katalogu"

if fileManager.fileExists(atPath: path) {
    print("Katalog istnieje")
} else {
    print("Katalog nie istnieje")
}
```

W powyższym przykładzie, na początku tworzymy obiekt klasy FileManager i przypisujemy go do stałej fileManager. Następnie definiujemy zmienną path jako ścieżkę do naszego katalogu, którego istnienie chcemy sprawdzić. W warunku if wywołujemy metodę fileExists(atPath:) na fileManagerze, przekazując do niej naszą zmienną path. W zależności od zwróconej wartości, wyświetlamy odpowiedni komunikat.

## Deep Dive
Klasa FileManager udostępnia również inne przydatne metody do sprawdzania katalogów, takie jak fileExists() czy fileExists(atPath: isDirectory:). Metoda fileExists() zwraca wartość logiczną określającą, czy podana ścieżka jest do pliku lub katalogu, natomiast fileExists(atPath: isDirectory:) pozwala na określenie, czy podana ścieżka wskazuje na plik czy katalog.

Należy również pamiętać, że przy sprawdzaniu ścieżki do katalogu można użyć względnej lub absolutnej ścieżki. Względna ścieżka jest określona od aktualnego katalogu, w którym znajduje się nasz program, natomiast absolutna ścieżka jest pełną ścieżką do pliku lub katalogu. Ważne jest, aby dostarczyć odpowiednią ścieżkę do metody fileExists(), w przeciwnym razie zwrócona wartość może być niepoprawna.

## Zobacz także
- [Dokumentacja klasy FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Przewodnik po podstawach Swift](https://www.raywenderlich.com/4919757-a-swift-tutorial-for-complete-beginners)

Dzięki temu artykułowi powinieneś być w stanie samodzielnie sprawdzać istnienie katalogów w Swifie. Pamiętaj, aby regularnie wykonywać takie sprawdzenia w swoim kodzie, aby zapewnić jego poprawność i niezawodność. Powodzenia!