---
title:    "Go: Odczyt pliku tekstowego"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanowiłeś się kiedyś, dlaczego w programowaniu ważne jest umiejętność czytania plików tekstowych? Poznaj powody w tym artykule!

## Jak To Zrobić

Zaczniemy od prostego przykładu, w którym wyświetlimy zawartość pliku tekstowego za pomocą języka Go. W poniższym kodzie wykorzystamy pakiet 'io/ioutil', który pozwala na wygodne czytanie plików.

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	file, err := ioutil.ReadFile("plik.txt") // otwarcie pliku tekstowego
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println(string(file)) // wyświetlenie zawartości pliku
}
```
Output:
```
To jest przykładowy plik tekstowy.
Każda linijka jest oddzielona znakiem nowej linii.
Czytanie plików tekstowych może być przydatne w wielu sytuacjach.
```

Teraz spróbujmy nieco bardziej zaawansowanego podejścia - chcemy odczytać tylko wybrane linijki z pliku tekstowego. W tym celu wykorzystamy funkcję 'ReadString' z pakietu 'bufio'.

```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("plik.txt") // otwarcie pliku
	if err != nil {
		fmt.Println(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file) // utworzenie skanera
	for scanner.Scan() {
		if scanner.Text() == "Każda linijka jest oddzielona znakiem nowej linii." || scanner.Text() == "To jest przykładowy plik tekstowy." {
			fmt.Println(scanner.Text()) // wyświetlenie wybranych linijek
		}
	}
}
```
Output:
```
To jest przykładowy plik tekstowy.
Każda linijka jest oddzielona znakiem nowej linii.
```

## Głębsza Analiza

Czytanie plików tekstowych jest jedną z podstawowych czynności w programowaniu, ponieważ pozwala nam na przetwarzanie dużej ilości danych w łatwy sposób. W języku Go występują różne metody odczytywania plików, jak widać w przedstawionych przykładach. Istnieje także możliwość ręcznego przetwarzania danych znak po znaku, jednakże wykorzystanie pakietów 'io/ioutil' lub 'bufio' jest zdecydowanie wygodniejsze i efektywniejsze.

## Zobacz Również

- [Dokumentacja pakietu 'io/ioutil'](https://golang.org/pkg/io/ioutil/)
- [Dokumentacja pakietu 'bufio'](https://golang.org/pkg/bufio/)
- [Przykłady kodu dla czytania plików tekstowych w języku Go](https://gobyexample.com/reading-files)