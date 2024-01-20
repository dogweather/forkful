---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie pliku tekstowego oznacza po prostu odczytanie zawartości danego pliku tekstowego. Programiści robią to, aby manipulować danymi, analizować je lub wprowadzać do swoich programów.

## Jak to zrobić:

Poniżej znajduje się prosty kod w Go do odczytu pliku tekstowego:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	data, err := ioutil.ReadFile("test.txt")
	if err != nil {
		fmt.Println("Błąd:", err)
		return
	}

	fmt.Println("Zawartość pliku:", string(data))
}
```
Załóżmy, że zawartość `test.txt` to `Witaj, świecie!`, wynik wyglądałby następująco:

```Go
Zawartość pliku: Witaj, świecie!
```

## Głębsze zagadnienia

Czytanie plików tekstowych ma wiele zastosowań, od prostych skryptów do zaawansowanych analiz danych. W Go, mamy kilka alternatyw dla `ioutil.ReadFile()`, takie jak `os.Open()` i `bufio.Scanner()`, które mogą być bardziej efektywne dla dużych plików.

Chociaż `ioutil.ReadFile()` jest wygodne, ładuje cały plik do pamięci, co może nie być efektywne dla bardzo dużych plików. Alternatywą może być użycie `os.Open()` w połączeniu z `bufio.Scanner()`.

Kiedy odczytujemy plik tekstowy, Go otwiera strumień do pliku, a następnie odczytuje zawartość pliku bajt po bajcie.

## Zobacz także

Poniżej znajdują się linki do szczegółowych informacji na temat odczytu plików tekstowych w Go:

1. Oficjalna Dokumentacja Go: https://golang.org/pkg/io/ioutil/#ReadFile
2. Przewodnik po Go zaawansowanym: https://gobyexample.com/reading-files
3. Tutorial: Jak odczytać plik w Go: https://www.socketloop.com/tutorials/golang-read-a-file-example