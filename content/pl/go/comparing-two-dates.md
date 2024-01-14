---
title:    "Go: Porównywanie dwóch dat"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Dlaczego porównywanie dwóch dat jest ważne w programowaniu? 

W programowaniu często spotykamy się z koniecznością porównywania dat. Może to być przydatne przy sortowaniu danych, wyświetlaniu najnowszych informacji lub tworzeniu programów z wykorzystaniem harmonogramów. W tym wpisie dowiesz się, jak porównywać daty w języku Go.

## Jak używać funkcji do porównywania dat w Go

Porównywanie dat w Go jest proste i wygodne dzięki wbudowanemu pakietowi `time`. Możemy użyć funkcji `Before()` i `After()` aby sprawdzić, czy jedna data występuje przed lub po drugiej.  Oto przykładowy kod w Go:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t1 := time.Date(2020, time.February, 1, 0,0,0,0, time.UTC)
    t2 := time.Date(2020, time.January, 1, 0,0,0,0, time.UTC)

    if t1.After(t2) {
        fmt.Println("T1 występuje po T2")
    } else if t1.Before(t2) {
        fmt.Println("T1 występuje przed T2")
    } else {
        fmt.Println("Daty są równe")
    }
}
```

Output:

```
T1 występuje po T2
```

Podczas tworzenia daty, możemy użyć funkcji `Date()` z pakietu time, określić rok, miesiąc, dzień, godzinę, minutę, sekundę i strefę czasową. Następnie możemy porównać daty za pomocą funkcji `After()` lub `Before()` i wyświetlić odpowiedni komunikat.

## Głębsze spojrzenie na porównywanie dat w Go

Funkcje `After()` i `Before()` porównują daty z dokładnością do milisekund, co jest wystarczające w większości przypadków. Jednak, jeśli potrzebujemy większej precyzji, możemy również skorzystać z funkcji `Equal()` do porównywania dat z dokładnością do nanosekund:

```Go
t1 := time.Date(2020, time.February, 1, 0,0,0,0, time.UTC)
t2 := time.Date(2020, time.February, 1, 0,0,0,1, time.UTC)

if t1.Equal(t2) {
    // wykona się ten kod gdy daty są równe
}
```

Możemy również porównać daty z różnymi strefami czasowymi, używając funkcji `In()`. Przykładowo, porównując datę ze strefy GMT z datą ze strefy czasowej Warszawa, musimy użyć funkcji `In()` aby przekonwertować tę datę na odpowiednią strefę czasową.

```Go
t1 := time.Date(2020, time.February, 1, 0,0,0,0, time.UTC)
t2 := time.Date(2020, time.February, 1, 0,0,0,0, time.LoadLocation("Europe/Warsaw"))

if t1.In(time.LoadLocation("Europe/Warsaw")) == t2 {
    // wykona się ten kod gdy daty są równe
}
```

Podczas porównywania dwóch dat, ważne jest również pamiętanie o strefie czasowej, w której zostały one utworzone. Może to wpłynąć na wynik porównania, dlatego ważne jest przygotowanie odpowiednich danych przed wykonaniem porównania dat.

# Zobacz również

- Dokumentacja pakietu `time` w języku Go: https://golang.org/pkg/time/
- Przewodnik po porównywaniu dat w języku Go: https://www.calhoun.io/comparing-times-in-go/