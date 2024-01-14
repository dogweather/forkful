---
title:                "Swift: Tworzenie pliku tymczasowego"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest ważną umiejętnością w programowaniu Swift, ponieważ pozwala na manipulację i przechowywanie danych w czasie działania programu. Jest to szczególnie przydatne, gdy chcemy zapisać dane, które potrzebujemy tylko na krótki okres czasu, a następnie je usunąć.

## Jak to zrobić

Aby utworzyć plik tymczasowy w Swift, należy użyć funkcji `TemporaryDirectory` z klasy `FileManager`. Następnie możemy użyć jej właściwości `.url` w celu utworzenia ścieżki do pliku tymczasowego. Kod może wyglądać następująco:

```Swift
let fileManager = FileManager()
let temporaryDirectory = fileManager.temporaryDirectory
let tempFileURL = temporaryDirectory.url.appendingPathComponent("nazwa_pliku.txt")
```

Możemy również ustawić odpowiednie opcje, takie jak prefiks i sufiks pliku, aby lepiej go identyfikować oraz jego rozszerzenie, jeśli to konieczne. Następnie możemy zacząć zapisywać dane do pliku tymczasowego, korzystając z funkcji `write()` lub `append()`.

```Swift
// zapisywanie danych do pliku
let dataToWrite = "To jest przykładowy tekst, który chcemy zapisać do pliku tymczasowego."
do {
    try dataToWrite.write(to: tempFileURL, atomically: true, encoding: .utf8)
} catch {
    print("Wystąpił błąd podczas zapisywania danych do pliku tymczasowego: \(error)")
}

// dodawanie danych do już istniejącego pliku
let dataToAdd = "\nKolejna linijka tekstu."
do {
    let fileHandle = try FileHandle(forWritingTo: tempFileURL)
    fileHandle.seekToEndOfFile()
    fileHandle.write(dataToAdd.data(using: .utf8)!)
} catch {
    print("Wystąpił błąd podczas dodawania danych do pliku tymczasowego: \(error)")
}
```

Po zakończeniu pracy z plikiem tymczasowym, powinniśmy go usunąć, aby nie zajmował niepotrzebnie miejsca. Możemy to zrobić przy użyciu funkcji `removeItem()` z klasy `FileManager`.

```Swift
do {
    try fileManager.removeItem(at: tempFileURL)
} catch {
    print("Wystąpił błąd podczas usuwania pliku tymczasowego: \(error)")
}
```

## Głębsza analiza

Istnieje wiele możliwości manipulacji plikiem tymczasowym w Swift, w zależności od naszych potrzeb. Możemy na przykład odczytywać i wyświetlać dane z pliku tymczasowego, a także tworzyć foldery tymczasowe i przechowywać w nich wiele plików. Pamiętajmy, że plik tymczasowy może zostać automatycznie usunięty po zakończeniu działania programu, dlatego warto użyć funkcji `Timer`, aby zapewnić, że nasz plik pozostanie widoczny przez określony okres czasu.

## Zobacz też

- Oficjalna dokumentacja Apple na temat tworzenia i manipulacji plikami tymczasowymi w Swift: https://developer.apple.com/documentation/foundation/filemanager
- Przykładowy projekt z wykorzystaniem pliku tymczasowego w Swift: https://github.com/JohnSundell/Files/tree/master/Sources/Files
- Dyskusja na temat tworzenia plików tymczasowych w języku Swift na forum Stack Overflow: https://stackoverflow.com/questions/45043336/recreating-a-temporary-file-each-time-it-is-used-in-swift