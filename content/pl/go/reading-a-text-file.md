---
title:                "Go: Odczytywanie pliku tekstowego"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/chciałaś przeczytać plik tekstowy w swoim programie Go? Jest to przydatna umiejętność, którą można wykorzystać w różnych scenariuszach programistycznych. W tym artykule omówimy, dlaczego warto nauczyć się czytać pliki tekstowe w Go i jak można to zrobić.

## Jak To Zrobić

Aby odczytać plik tekstowy w Go, musimy najpierw użyć pakietu "os" i jego funkcji "Open". Następnie możemy wykorzystać funkcję "Read" z pakietu "bufio", aby czytać plik linia po linii. Poniżej przedstawiamy kod przykładowy:

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("plik.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		panic(err)
	}
}
```

Powyższy kod otwiera plik "plik.txt" i czyta go linia po linii, drukując każdą linię w konsoli. Możemy również wykonać różne operacje na każdej linii, w zależności od naszych potrzeb.

Przykładowy "plik.txt":

```
Pierwsza linia
Druga linia
Trzecia linia
```

Wyjście z programu powinno wyglądać następująco:

```
Pierwsza linia
Druga linia
Trzecia linia
```

## Deep Dive

Podczas czytania pliku tekstowego w Go musimy pamiętać o dwóch ważnych rzeczach - obsłudze błędów i zamknięciu pliku. W zaimplementowanym kodzie używamy "defer", aby zamknąć plik automatycznie zaraz po zakończeniu funkcji.

Co więcej, możemy także wykorzystać pakiet "ioutil" i jego funkcję "ReadFile" do odczytania całego pliku jednym poleceniem. W ten sposób możemy uniknąć pętli i skanera, ale musimy pamiętać, że ta metoda będzie działać tylko dla niewielkich plików tekstowych ze względu na wykorzystanie pamięci.

## Zobacz Również

- Dokumentacja pakietu "os": https://golang.org/pkg/os/
- Dokumentacja pakietu "bufio": https://golang.org/pkg/bufio/
- Dokumentacja pakietu "ioutil": https://golang.org/pkg/io/ioutil/