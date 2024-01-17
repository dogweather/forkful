---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Go: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?
W Go (aktualna wersja), sprawdzanie czy dany katalog istnieje jest procesem, w którym programista stwierdza, czy dana ścieżka do katalogu jest dostępna w systemie plików. Jest to ważne w celu zapewnienia poprawnego działania programu, np. przy odczytywaniu lub zapisywaniu plików.

## Jak to zrobić:
Sprawdzenie czy katalog istnieje w Go jest bardzo proste. Można to zrobić przy użyciu funkcji `os.Stat()` i sprawdzenia czy występuje błąd przy próbie dostępu do ścieżki. Poniżej przedstawiony jest przykład kodu oraz ewentualne wyjście.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {

	// podaj ścieżkę do sprawdzenia
	path := "ściezka/do/katalogu"

	// sprawdź czy ścieżka istnieje przy użyciu os.Stat()
	if _, err := os.Stat(path); err == nil {
		// katalog istnieje
		fmt.Println("Katalog istnieje!")
	} else {
		// wystąpił błąd lub katalog nie istnieje
		fmt.Println("Katalog nie istnieje lub wystąpił błąd.")
	}
}
```

### Przykładowe wyjście:
```
Katalog istnieje!
```

## Pogłębione zagadnienia:
Sprawdzanie czy katalog istnieje to stosunkowo prosta operacja, ale warto mieć na uwadze trochę pogłębionych informacji na ten temat. Przede wszystkim, funkcja `os.Stat()` ma także swoje odpowiedniki w innych językach programowania, np. `file_exists()` w PHP czy `exists()` w Pythonie. Ponadto, można także użyć innej funkcji w Go, mianowicie `os.IsNotExist()`, która sprawdzi czy dany błąd jest spowodowany brakiem istnienia pliku lub katalogu. Warto także pamiętać, że nie tylko katalogi można sprawdzać w ten sposób, ale także pliki.

## Zobacz także:
- Dokumentacja funkcji `os.Stat()` w języku Go: https://golang.org/pkg/os/#Stat
- Inne sposoby na sprawdzanie istnienia katalogów w Go: https://golang.org/pkg/os/#example_File_nameIsDirectory
- Poradnik na temat operacji na plikach i katalogach w języku Go: https://golang.org/doc/tutorials/io/