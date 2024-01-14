---
title:                "Go: Odczyt pliku tekstowego"
simple_title:         "Odczyt pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tekstowe pliki są powszechnym formatem przechowywania danych i często są wykorzystywane w programowaniu. W tym artykule dowiesz się, jak w języku Go można łatwo odczytać dane z pliku tekstowego.

## Jak to zrobić

W języku Go, można użyć wbudowanej funkcji "os.Open()" do otwarcia pliku tekstowego. Następnie, używając funkcji "bufio.NewScanner()", możemy odczytywać linie tekstu z pliku. Poniżej przedstawiony jest przykładowy kod, który otwiera plik "hello.txt" i odczytuje jego zawartość:

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("hello.txt")
	if err != nil {
		fmt.Println("Nie można otworzyć pliku:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
}
```

Przykładowy plik "hello.txt" zawiera następujące dane:

```
Witaj w świecie Go!
To jest wartość testowa.
```

Wyjście programu będzie wyglądać następująco:

```
Witaj w świecie Go!
To jest wartość testowa.
```

## Wnikliwe spojrzenie

Funkcje "os.Open()" i "bufio.NewScanner()" są bardzo przydatne, ale warto zauważyć, że nie są one jedynymi sposobami na odczytywanie plików tekstowych w języku Go. Istnieją również inne biblioteki, takie jak "ioutil" lub "file", które mogą być wykorzystane w podobny sposób. Wybór odpowiedniej metody zależy od konkretnego przypadku użycia.

## Zobacz również

- [Dokumentacja języka Go na temat odczytywania plików](https://golang.org/pkg/os/#Open)
- [Poradnik odczytywania plików tekstowych w Go](https://gobyexample.com/reading-files)
- [Tutorial aplikacji konsolowej w języku Go](https://tutorialedge.net/golang/reading-console-input-golang/)