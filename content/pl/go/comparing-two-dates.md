---
title:                "Go: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego porównywać daty?

Porównywanie dat jest częstym wyzwaniem dla programistów, szczególnie dla tych, którzy pracują w języku Go. Dzięki temu krótkiemu przewodnikowi nauczysz się, jak porównywać daty w Go i uniknąć frustracji przy próbie rozwiązania tego problemu.

## Jak to zrobić?

By porównać dwie daty w Go, musisz najpierw skonwertować je na typ ```time.Time```. Następnie wykorzystaj operator `>` lub ` <` aby porównać daty i sprawdzić, która jest wcześniejsza lub późniejsza.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Utwórz przykładowe daty
    date1 := time.Date(2021, 3, 15, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, 6, 1, 0, 0, 0, 0, time.UTC)

    // Porównaj daty
    if date1 > date2 {
        fmt.Println(date1, "jest późniejsza niż", date2)
    } else if date2 > date1 {
        fmt.Println(date2, "jest późniejsza niż", date1)
    } else {
        fmt.Println(date1, "i", date2, "są takie same")
    }
}
```

Output:
```Go
2021-06-01 00:00:00 +0000 UTC jest późniejsza niż 2021-03-15 00:00:00 +0000 UTC
```

## Deep Dive

Porównywanie dat w Go może być nieco skomplikowane ze względu na różnice w sposobie obsługi czasu w różnych strefach czasowych. Dlatego najlepiej pracować z datami w jednej jednostce czasu, na przykład w UTC lub w lokalnej strefie czasowej. Możesz użyć metody `UTC()` lub `Local()` by skonwertować datę na wybraną strefę czasową.

Ponadto, jeśli chcesz porównywać również godziny i minuty, musisz wykorzystać metody `Truncate()` lub `Round()` by wyeliminować różnice w czasie z uwagi na precyzję.

## Zobacz także
- [Pakiet time w języku Go](https://pkg.go.dev/time)
- [Porównywanie dat w języku Go](https://golang.org/src/time/example_test.go)