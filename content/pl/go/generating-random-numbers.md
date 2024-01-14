---
title:                "Go: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważnym elementem wielu programów. Może posłużyć do symulacji, testowania, lub prostego losowania losowych elementów.

## Jak to zrobić

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Ustawienie ziarna generatora losowych liczb
	rand.Seed(time.Now().UnixNano())

	// Generowanie pojedynczej losowej liczby całkowitej z zakresu 0-10
	randomInt := rand.Intn(11)
	fmt.Println(randomInt)

	// Generowanie pojedynczej losowej liczby zmiennoprzecinkowej z zakresu 0-1
	randomFloat := rand.Float64()
	fmt.Println(randomFloat)

	// Generowanie losowej liczby z zakresu 10-20
	randomRange := rand.Intn(11) + 10
	fmt.Println(randomRange)

	// Generowanie listy 10 losowych liczb całkowitych z zakresu 1-100
	for i := 0; i < 10; i++ {
		fmt.Print(rand.Intn(100) + 1)
		fmt.Print(" ")
	}
}
```

Wynik:

7
0.7859221557077516
17
20 62 91 50 70 86 80 57 76 82

## Deep Dive

Generowanie losowych liczb jest w rzeczywistości wykonywane przy pomocy pseudolosowego generatora. Oznacza to, że podczas gdy wyglądają one na losowe, faktycznie są one generowane przy użyciu ustalonego algorytmu. Aby uzyskać jeszcze bardziej losowe liczby, często używa się tzw. ziarna generatora (np. na podstawie aktualnego czasu), które zmienia początkowy stan algorytmu. W ten sposób, za każdym razem, gdy generator jest uruchamiany, wyniki są różne.

## Zobacz również

- [Oficjalna dokumentacja języka Go dotycząca generowania liczb losowych](https://golang.org/pkg/math/rand/)
- [Przykładowy program generujący losowe liczby w Go](https://www.calhoun.io/creating-random-numbers-in-go/)
- [Wideo wyjaśniające, jak działa pseudolosowy generator w języku Go](https://www.youtube.com/watch?v=3vQfzhU-nY4)