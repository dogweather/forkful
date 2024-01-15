---
title:                "Porównywanie dwóch dat"
html_title:           "Go: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest niezwykle przydatną umiejętnością, szczególnie w przypadku skomplikowanych aplikacji, które wymagają obsługi różnych zdarzeń w różnych chwilach. Dzięki umiejętności porównywania dat możesz łatwo określić, które wydarzenia już się wydarzyły, a które dopiero nadejdą. W tym artykule dowiesz się, jak porównywać daty w języku Go i jak to może ułatwić Twoją pracę.

## Jak to zrobić

Aby porównać dwie daty w języku Go, możesz użyć funkcji `Equal` z pakietu `time`. Przykładowy kod wyglądałby następująco:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dateOne := time.Date(2021, 9, 15, 0, 0, 0, 0, time.UTC)
	dateTwo := time.Date(2021, 9, 16, 0, 0, 0, 0, time.UTC)
	
	if dateOne.Equal(dateTwo) {
		fmt.Println("Podane daty są takie same!")
	} else {
		fmt.Println("Podane daty są różne.")
	}
}
```

Oczekiwanym wynikiem tego kodu jest wypisanie na ekranie napisu "Podane daty są różne.", ponieważ wcześniej zdefiniowane daty są właśnie różne. Korzystając z funkcji `Equal`, możesz więc prosto i szybko porównać dwie daty w języku Go.

## Deep Dive

Funkcja `Equal` porównuje dwie daty do sekundy, dlatego jeśli chcesz również uwzględnić milisekundy, musisz użyć funkcji `Equal` na typie `time.Time`. Ponadto, warto wspomnieć, że w języku Go daty są reprezentowane przez typ `time.Time`, który przechowuje informacje o dacie, czasie oraz strefie czasowej. Dzięki temu możesz porównywać nie tylko daty w jednej strefie czasowej, ale również w różnych, co jest przydatne w przypadku globalnych aplikacji.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat porównywania dat w języku Go, możesz zapoznać się z następującymi artykułami:

- [Porównywanie dat w języku Go](https://golangbyexample.com/time-comparison-in-golang/)
- [Dokumentacja języka Go - pakiet time](https://golang.org/pkg/time/)

Dzięki umiejętności porównywania dat w języku Go, możesz ułatwić sobie pracę i uniknąć błędów związanych z obsługą różnych zdarzeń czasowych w swoich aplikacjach.