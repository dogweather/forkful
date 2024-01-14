---
title:    "Swift: Sprawdzenie, czy istnieje katalog"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Swift i pracujesz z plikami i folderami, możesz chcieć sprawdzić, czy dany folder istnieje, zanim zaczniesz przetwarzać lub przechowywać w nim pliki. W tym artykule dowiesz się, jak to zrobić w prosty sposób.

## Jak to zrobić

Sprawdzenie, czy dany folder istnieje, wymaga użycia klasy `FileManager`. Możemy użyć metody `fileExists(atPath:)`, która zwraca wartość logiczną, czy dany plik lub folder istnieje w podanej ścieżce. Przykładowy kod może wyglądać następująco:

```Swift
let fileManager = FileManager.default
let documentsFolder = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!

let folderPath = documentsFolder.appendingPathComponent("Moje pliki")

if fileManager.fileExists(atPath: folderPath.path) {
    print("Dany folder istnieje!")
} else {
    print("Dany folder nie istnieje.")
}
```

Jeśli folder `Moje pliki` istnieje w folderze dokumentów, w konsoli zostanie wyświetlony komunikat "Dany folder istnieje!". W przeciwnym razie zostanie wyświetlona informacja, że folder nie istnieje.

## Deep Dive

Metoda `fileExists(atPath:)` jest bardzo przydatna, ale nie należy jej używać, gdy chcemy dokładnie sprawdzić, czy dany folder istnieje. Może się zdarzyć, że folder zostanie usunięty lub zmieniona będzie jego ścieżka, a ta metoda nadal zwróci wartość `true`, ponieważ znajdzie dany folder w pamięci podręcznej.

Aby dokładnie sprawdzić, czy dany folder istnieje, możemy użyć metody `contentsOfDirectory(atPath:)`, która zwraca listę plików i folderów w podanej ścieżce. Jeśli poziom zagnieżdżenia będzie równy 1 (czyli nie ma podfolderów), a lista będzie pusta, to oznacza, że dany folder istnieje i jest pusty. Przykładowy kod może wyglądać tak:

```Swift
let fileManager = FileManager.default
let documentsFolder = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!

let folderPath = documentsFolder.appendingPathComponent("Moje pliki")

do {
    let contents = try fileManager.contentsOfDirectory(atPath: folderPath.path)
    if contents.count == 0 {
        print("Dany folder istnieje i jest pusty.")
    } else {
        print("Dany folder istnieje i zawiera \(contents.count) elementów.")
    }
} catch {
    print("Wystąpił błąd podczas pobierania zawartości folderu.")
}
```

## Zobacz także

- Dokumentacja klasy `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Praca z plikami i folderami w Swift: https://www.hackingwithswift.com/read/0/overview