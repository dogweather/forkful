---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat polega na sprawdzeniu, która z nich jest wcześniejsza lub późniejsza. Programiści robią to, aby przetwarzać informacje zależne od czasu w sensowny sposób.

## Jak to zrobić:

Porównanie dwóch dat w języku Go jest proste. Skorzystaj z istniejących pakietów `time`.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Tworzenie dwóch różnych dat
    time1 := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)
    time2 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)

    // Porównanie
    if time1.Before(time2) {
        fmt.Println("2000 jest przed 2020")
    } 
    if time2.After(time1) {
        fmt.Println("2020 jest po 2000")
    }
}
```

Wynik na wyjściu powinien wyglądać tak:
```
2000 jest przed 2020
2020 jest po 2000
```

## Głębsze spojrzenie 

Pomysł porównywania dat pochodzi jest starożytny, ale stworzenie Go od podstaw to szczególna cecha golang. Alternatywami dla `time.Before()` i `time.After()`, jest możliwość odczytania wartości `.UnixNano()` dwóch dat i porównania tych wartości.

**Implementacja szczegółów**: Metoda `time.Before()` porównuje daty porównując interne reprezentacje czasu którą jest liczba nanosekund od epoki.

## Zobacz także:

1. Dokumentacja pakietu czasu Go: https://golang.org/pkg/time/
2. Wprowadzenie do Go na stronie: https://tour.golang.org/welcome/1
3. Comparing dates in Go: https://yourbasic.org/golang/compare-dates/