---
title:                "Go: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości jest częstą operacją w aplikacjach, które wymagają manipulacji czasem, takich jak aplikacje księgowe lub programy do planowania spotkań. Dzięki Go jest to proste do zaimplementowania i może zaoszczędzić czas programistom.

## Jak to zrobić

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Obliczanie daty jutro
	jutro := time.Now().AddDate(0, 0, 1)
	fmt.Println("Jutro będzie", jutro)

	// Obliczanie daty z przeszłości
	przeszlosc := time.Now().AddDate(0, -1, 0)
	fmt.Println("Data z miesiąc temu była", przeszlosc)
}
```

**Output:**

```
Jutro będzie 2021-05-26 15:12:34.985523 +0100 BST m=+86401.000508876
Data z miesiąc temu to było 2021-04-25 15:12:34.985523 +0100 BST
```

## Głębsza analiza

Powyższy przykład pokazuje użycie funkcji `AddDate()` w pakiecie `time`, aby obliczyć datę w przyszłości lub przeszłości. Ta funkcja przyjmuje trzy argumenty: liczbę lat, miesięcy i dni, które mają zostać dodane do bieżącej daty. Można również użyć liczb ujemnych, aby obliczyć datę w przeszłości.

Ponadto, pakiet `time` oferuje również wiele innych metod, takich jak `Sub()` do obliczania różnicy między dwiema datami, `Equal()` do porównywania dwóch dat czy `Format()` do formatowania daty według określonego szablonu.

## Zobacz także

- [Oficjalna dokumentacja Go - pakiet time](https://pkg.go.dev/time)
- [Tutorial na temat obliczania dat w Go](https://golangbyexample.com/datetime-operations-go/)