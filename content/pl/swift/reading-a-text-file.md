---
title:    "Swift: Wczytywanie pliku tekstowego."
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak odczytać tekstowy plik w języku Swift? Może masz do zrobienia projekt, który wymaga wczytania danych z pliku tekstowego lub chcesz po prostu poszerzyć swoją wiedzę na temat programowania w Swift? W tym artykule dowiesz się, jak to zrobić.

## Jak to zrobić

Do odczytywania plików tekstowych w języku Swift potrzebujemy skorzystać z klasy `FileManager` i jej metody `contents(atPath:)`. Poniżej znajduje się przykładowy kod w języku Swift:

```swift
do {
    if let filePath = Bundle.main.path(forResource: "plik_tekstowy", ofType: "txt") { // Ustawienie ścieżki do pliku
        let fileData = try Data(contentsOf: URL(fileURLWithPath: filePath)) // Odczytanie danych z pliku
        let text = String(data: fileData, encoding: .utf8) ?? "" // Konwersja danych na typ String
        print(text) // Wyświetlenie zawartości pliku
    }
} catch {
    print(error) // Obsługa błędu
}
```

Powyższy kod najpierw sprawdza, czy udało się ustawić ścieżkę do pliku tekstowego. Następnie odczytuje dane z pliku i konwertuje je na typ String. Na koniec wyświetla zawartość pliku w konsoli. Ważne jest też obsłużenie potencjalnych błędów za pomocą konstrukcji `try-catch`.

## Deep Dive

W języku Swift do odczytywania plików tekstowych możemy także wykorzystać klasę `FileHandle`. Pozwala ona na odczytywanie danych z pliku w sposób bardziej wydajny, szczególnie w przypadku duży plików tekstowych. Poniżej przedstawiony jest przykład kodu z wykorzystaniem `FileHandle`:

```swift
if let file = FileHandle(forReadingAtPath: "plik_tekstowy.txt") { // Ustawienie ścieżki do pliku
    let data = file.readDataToEndOfFile() // Odczytanie danych z pliku
    let text = String(data: data, encoding: .utf8) ?? "" // Konwersja danych na typ String
    print(text) // Wyświetlenie zawartości pliku
    file.closeFile() // Zamknięcie pliku
}
```

W tym przypadku wykorzystaliśmy metodę `readDataToEndOfFile()`, która odczytuje wszystkie dane z pliku. Pamiętajmy też o zamknięciu pliku po jego użyciu za pomocą metody `closeFile()`.

## Zobacz także

- [Dokumentacja Apple - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Dokumentacja Apple - FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Odczyt i zapis plików na platformie iOS](https://www.raywenderlich.com/10258852-ios-file-management-tutorial-getting-started)