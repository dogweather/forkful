---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowego pliku to sposób na zapisanie danych, które są potrzebne tylko przez określony krótki okres czasu. Programiści robią to, aby zaoszczędzić miejsce na dysku twardym i zwiększyć wydajność swojego kodu.

## Jak to zrobić:

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    tempFile, err := ioutil.TempFile("temp", "goexample.*.txt")
    if err != nil {
        panic(err)
    }

    defer tempFile.Close()

    text := []byte("Przykładowy tekst")
    if _, err = tempFile.Write(text); err != nil {
        panic(err)
    }

    fmt.Printf("Tymczasowy plik: %s\n", tempFile.Name())
}
```

Po uruchomieniu powyższego kodu zostanie utworzony nowy plik tymczasowy z przykładowym tekstem. Nazwa pliku zostanie wyświetlona na konsoli.

## Podróż w głąb materiału:

Tworzenie plików tymczasowych to klasyczna technika programistyczna, która ma swoje korzenie w czasach, kiedy przestrzeń dyskowa była cenna. Alternatywą dla tej techniki jest użycie struktur danych przechowywanych w pamięci, ale to nie zawsze jest możliwe z powodu ograniczeń pamięci.

Istotnym aspektem implementacji `ioutil.TempFile` jest to, że nie tylko tworzy plik tymczasowy, ale także otwiera go do odczytu i zapisu. Zwraca obiekt `*os.File`, który programista może używać do manipulowania plikiem.

## Zobacz także:

1. Dokumentacja Golang na temat tworzenia plików tymczasowych: [https://golang.org/pkg/io/ioutil/#TempFile](https://golang.org/pkg/io/ioutil/#TempFile)