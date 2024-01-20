---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Tworzenie tymczasowego pliku to proces, w którym program tworzy plik o krótkim czasie życia, służący do przechowywania danych na czas działania programu. Programiści robią to, aby efektywnie manipulować danymi i zminimalizować użycie pamięci.

## Jak to zrobić:

Siegnijmy po domyślny menedżer plików w Swift:

```Swift
let fileManager = FileManager.default
```

Teraz, tworzymy ścieżkę dla naszego tymczasowego pliku:

```Swift
let tempDirectory = NSTemporaryDirectory()
let tempFile = tempDirectory.appending("temp.txt")
```
Możemy teraz zapisać jakiś tekst do naszego tymczasowego pliku:

```Swift
do {
    try "Cześć, Świecie!".write(toFile: tempFile, atomically: true, encoding: .utf8)
} catch {
    print("Nie udało się zapisać do pliku: \(error)")
}
```
## Pogłębione informacje

Historia: Pliki tymczasowe są z nami od dawna, używane są w każdym rodzaju systemów operacyjnych. Idea stworzenia tymczasowego pliku pochodzi z konieczności minimalizowania użycia pamięci i zasobów systemu.

Alternatywy: Można również używać innych metod przechowywania danych, takich jak bazy danych czy pamięć Cache, jednak każda z nich ma swoje plusy i minusy.

Szczegóły implementacji: Waistliwe jest tworzenie pliku tymczasowego w Swift. Tworzymy tymczasowy katalog, a potem dodajemy do niego nasz plik. Zastosowanie metody `NSTemporaryDirectory` zapewnia nam ścieżkę do katalogu tymczasowego.

## Zobacz też:

1. [Podręcznik: Zarządzanie plikami w Swift](https://developer.apple.com/documentation/foundation/filemanager)