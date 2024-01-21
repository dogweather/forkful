---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:54:36.130339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie pliku tekstowego to operacja, w której program wczytuje zawartość pliku do pamięci. Programiści robią to, aby przetworzyć dane, skonfigurować program, lub załadować zasoby.

## Jak to zrobić:
```Go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    // Otwieranie pliku
    file, err := os.Open("przykladowy.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    // Czytanie pliku linia po linii
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```
### Wyjście próbkowe:
```
Pierwsza linia tekstu
Druga linia tekstu
Trzecia linia tekstu
```

## Deep Dive:
Czytanie plików w Go jest proste i wydajne. W przeszłości, korzystano z bardziej niskopoziomowych funkcji w językach takich jak C. Go oferuje pakiet `os` do zarządzania plikami i `bufio` do efektywnego czytania strumieniowych danych.

Alternatywy:
- `ioutil.ReadAll` - do wczytania całego pliku, ale mniej wydajne dla dużych plików.
- `os.ReadFile` - nowy sposób w Go 1.16, czyta cały plik naraz.
- `io/ioutil` - pakiet został zdeprecjonowany w Go 1.16, ale nadal jest używany w starszych kodach.

Implementacja:
- Użycie `defer` do bezpiecznego zamknięcia pliku.
- `bufio.Scanner` rozdziela plik na linie, zamiast ładować całość, co jest efektywne dla dużych plików.

## Zobacz również:
- Dokumentacja Go na temat pakietu `os`: https://pkg.go.dev/os
- Dokumentacja Go na temat pakietu `bufio`: https://pkg.go.dev/bufio
- Artykuł o obsłudze plików w Go: https://go.dev/doc/io/ioutil-readall