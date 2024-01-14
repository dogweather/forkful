---
title:                "Swift: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak odczytać plik tekstowy w języku Swift? Odczytywanie pliku tekstowego jest niezbędnym procesem w wielu projektach programistycznych. W tym artykule dowiesz się, jak to zrobić w prosty sposób.

## Jak to zrobić

Aby odczytać plik tekstowy w języku Swift, musimy najpierw utworzyć instancję klasy `FileHandle`, która reprezentuje strumień danych pliku. Następnie musimy określić ścieżkę do pliku, którą przekażemy jako argument do metody `FileHandle(forReadingAtPath:)`. Gdy już mamy utworzony strumień, możemy użyć metody `readDataToEndOfFile()` do odczytania całego pliku lub `readData(ofLength:)` do odczytania określonej liczby bajtów. Oto przykładowy kod:

```Swift
let fileManager = FileManager.default
let path = "/Users/jan/documents/moj_plik.txt"

if fileManager.fileExists(atPath: path) {
  if let fileHandle = FileHandle(forReadingAtPath: path) {
    let fileData = fileHandle.readDataToEndOfFile()

    //przetwarzanie danych z pliku

    fileHandle.closeFile()
  } else {
    print("Nie można otworzyć pliku.")
  }
} else {
  print("Plik nie istnieje.")
}
```

Powyższy przykład pokazuje, jak w prosty sposób odczytać zawartość pliku tekstowego. Jednak możliwości są znacznie większe, ponieważ możemy również definiować specjalne kodowania dla pliku lub używać klas `InputStream` i `OutputStream` do odczytu i zapisu danych.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o odczytywaniu plików tekstowych w języku Swift, możesz zapoznać się z dokumentacją języka Swift lub przeczytać wpisy na blogach programistycznych. Ważne jest również, aby zapoznać się z różnymi metodami przetwarzania danych z plików tekstowych, takimi jak dzielenie tekstu na linie lub wyodrębnianie określonych informacji.

## Zobacz też

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [Blog na temat programowania w języku Swift](https://swiftwithmajid.com)
- [Przetwarzanie danych z plików tekstowych w języku Swift](https://www.appcoda.com/swift-read-write-file/)