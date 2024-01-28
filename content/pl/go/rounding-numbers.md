---
title:                "Zaokrąglanie liczb"
date:                  2024-01-26T03:46:12.026582-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb oznacza dostosowanie liczby do najbliższej liczby całkowitej lub określonego miejsca po przecinku. Robi się to, aby upraszczać wartości, uczynić je bardziej czytelnymi lub dopasować do pewnych ograniczeń, jak przy pracy z walutami.

## Jak to zrobić:
Pakiet `math` w języku Go to Twój przyjaciel do zaokrąglania. Użyj `math.Round`, `math.Floor` i `math.Ceil` dla uproszczenia:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Zaokrąglij do najbliższej liczby całkowitej
	fmt.Println("Floor:", math.Floor(number)) // Zaokrąglij w dół
	fmt.Println("Ceil: ", math.Ceil(number))  // Zaokrąglij w górę
}
```

Przykładowe wyjście:
```
Round: 3
Floor: 3
Ceil: 4
```

Dla konkretnych miejsc po przecinku pomnóż, zaokrąglij, a następnie podziel:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Zaokrąglone do 2 miejsc po przecinku:", roundToDecimalPlace(number, 2))
}
```

Przykładowe wyjście:
```
Zaokrąglone do 2 miejsc po przecinku: 3.14
```

## Podsumowanie
Zaokrąglanie liczb nie jest niczym nowym – sięga starożytnej matematyki, zawsze zmierzając do prostoty. `math.Round` w Go używa [zaokrąglania bankowego](https://pl.wikipedia.org/wiki/Zaokrąglanie), co oznacza, że 0,5 jest zaokrąglane do najbliższej parzystej liczby, redukując stronniczość, która mogłaby wpłynąć na sumy.

Liczby zmiennoprzecinkowe mogą być problematyczne z powodu ich binarnej reprezentacji, która może nie przedstawiać dokładnie wszystkich licz dziesiętnych. Podejście Go, jednak, w większości przypadków utrzymuje oczekiwane zachowanie.

Istnieją inne metody zaokrąglania, takie jak "zaokrąglij połowę w górę" lub "zaokrąglij połowę z dala od zera", ale to, co jest dostępne w standardowej bibliotece Go, jest od razu do użytku. Dla bardziej złożonych potrzeb, możesz potrzebować biblioteki stron trzecich lub opracować własne rozwiązanie.

## Zobacz także
- Pakiet `math` w Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Standard IEEE 754 dla arytmetyki zmiennoprzecinkowej (podstawa Go dla obsługi floatów): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Zrozumienie arytmetyki zmiennoprzecinkowej: ["Co każdy informatyk powinien wiedzieć o arytmetyce zmiennoprzecinkowej"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
