---
title:                "Tworzenie pliku tekstowego"
html_title:           "Swift: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Pisanie pliku tekstowego to po prostu tworzenie i zapisywanie tekstu w pliku na komputerze. Programiści często robią to, aby zachować ważne informacje lub wyniki swojego kodu, tak aby mogli do nich powrócić w przyszłości.

## Jak to zrobić:

```Swift
// Tworzenie nowego pliku tekstowego o nazwie "moj_plik.txt"
let nazwaPliku = "moj_plik.txt"
let tekst = "Ten tekst zostanie zapisany w pliku!"
 
do {
  // Używanie mechanizmu "try-catch" w przypadku, gdyby nie udało się utworzyć pliku.
  try tekst.write(toFile: nazwaPliku, atomically: false, encoding: String.Encoding.utf8)
}
catch {
  print("Nie udało się zapisać pliku!")
}
```

Powyższy kod utworzy nowy plik "moj_plik.txt" w folderze, w którym uruchomiono kod. Jeśli chcesz zmienić miejsce zapisu, możesz podać ścieżkę do odpowiedniego folderu w nazwie pliku.

## Głębsze wody:

- Pisanie pliku tekstowego jest często używane w celu zapisywania wyników obliczeń lub zmiennych, przetwarzania danych lub nawet tworzenia plików konfiguracyjnych dla innych aplikacji.
- Istnieje wiele alternatywnych sposobów na pisanie plików w języku Swift, takich jak używanie funkcji `FileHandle` lub `Data` zamiast `String`.
- Staraj się nie zapisywać ważnych danych w plikach tekstowych, ponieważ mogą one być narażone na utratę lub nadpisane przez inne aplikacje na twoim komputerze.

## Zobacz też:

- [Apple's documentation on writing files in Swift](https://developer.apple.com/documentation/foundation/data_writing)
- [A guide to file handling in Swift](https://medium.com/@jacobawenger/file-handling-in-swift-1f851551c2a6)
- [Apple's guide to using FileHandle for file operations in Swift](https://developer.apple.com/documentation/foundation/filehandle)