---
title:                "Swift: Tworzenie pliku tekstowego"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto tworzyć pliki tekstowe w języku Swift?

Pisanie plików tekstowych jest ważnym aspektem programowania w języku Swift. Pozwala nam zapisywać i odczytywać dane w prosty sposób, co może być bardzo przydatne w różnych aplikacjach. W tym wpisie dowiedz się dlaczego warto korzystać z plików tekstowych oraz jak to zrobić.

## Jak to zrobić?

Pierwszym krokiem jest otworzenie pliku do zapisu lub odczytu za pomocą klasy `FileHandle`. Następnie wykorzystując metodę `write` lub `read`, możemy zapisywać lub odczytywać dane z pliku. Poniżej przedstawione są przykładowe fragmenty kodu z wykorzystaniem biblioteki `Foundation`.

```Swift
// otwarcie pliku do zapisu
let file = FileHandle(forWritingAtPath: "dane.txt")

// zapisywanie danych do pliku
let data = "To jest przykładowy tekst do zapisania w pliku."
file?.write(data.data(using: .utf8)!)

// zamknięcie pliku
file?.close()

// otwarcie pliku do odczytu
let file2 = FileHandle(forReadingAtPath: "dane.txt")

// odczytywanie danych z pliku
let fileData = file2?.readDataToEndOfFile()

// konwersja danych na string
if let dataString = String(data: fileData!, encoding: .utf8) {
    print(dataString)
}

// zamknięcie pliku
file2?.close()
```

Powyższy kod pokazuje jak w prosty sposób można otworzyć plik do zapisu oraz odczytać z niego dane. Pamiętaj, że przed zapisaniem danych konieczne jest przekonwertowanie ich na typ `Data`, a przed odczytaniem - na `String`.

## Głębszy zanurzenie

Tworzenie plików tekstowych w języku Swift jest możliwe dzięki wykorzystaniu klasy `FileHandle` z biblioteki `Foundation`. Klasa ta umożliwia dostęp do plików na różne sposoby, jak np. odczytanie tylko części danych za pomocą metody `read(upToCount:)` lub przesunięcie kursora za pomocą metody `seek(toFileOffset:)`. Ponadto, klasa `FileHandle` dziedziczy po klasie `InputStream` i `OutputStream`, co pozwala na wygodne korzystanie z funkcjonalności związanych z przesyłaniem danych.

## Zobacz też

- [Dokumentacja Apple o klasie FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Inne sposoby na manipulowanie plikami tekstowymi w Swift](https://medium.com/swift-programming/swift-programing-working-with-strings-files-d4da5f6fb2c)
- [Przykładowy projekt wykorzystujący pliki tekstowe w Swift](https://github.com/karolaltamirano/swift-file-manager)