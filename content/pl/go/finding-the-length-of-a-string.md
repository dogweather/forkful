---
title:    "Go: Znajdowanie długości ciągu znaków"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości ciągu znaków jest jednym z podstawowych zadań programowania. W Go jest to prosta i wydajna operacja, która może być używana w wielu różnych przypadkach.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Go, możemy użyć wbudowanej funkcji `len()`, która zwraca ilość elementów w danym typie zmiennej. W przypadku stringów, ta funkcja będzie zwracać liczbę bajtów, a nie liczność znaków.

```Go
package main

import "fmt"

func main() {
	str := "Cześć, Jak się masz?"
	fmt.Println(len(str))
}
```

Output: `20`

Funkcja ta jest bardzo prosta w użyciu, a jej działanie jest szybkie i efektywne. Jednak trzeba pamiętać, że działa ona na podstawie liczby bajtów, więc jeśli używamy znaków z kodowania ASCII (jednobajtowego), to wszystko będzie działać poprawnie. Jednak w przypadku używania znaków z kodowania UTF-8 (wielobajtowego), to długość stringa może nie być równa liczbie znaków.

## Deep Dive

Dla osób, które chcą wiedzieć więcej o działaniu funkcji `len()` na stringach w Go, warto wspomnieć, że jest ona obsługiwana przez pakiet `unicode/utf8`. Ten pakiet zawiera różne funkcje, takie jak `RuneCount()`, które pozwalają na dokładne obliczenie ilości znaków w stringu, a nie tylko bajtów. Używanie tych funkcji jest szczególnie ważne w przypadku pracy z różnymi językami i alfabetami.

## Zobacz także

- [Dokumentacja funkcji len() w pakiecie `unicode/utf8`](https://golang.org/pkg/unicode/utf8/#RuneCount)
- [Krótki kurs języka Go dla początkujących](https://go-tour-pl.appspot.com/)
- [Społeczność języka Go w Polsce](https://golang.org.pl/)