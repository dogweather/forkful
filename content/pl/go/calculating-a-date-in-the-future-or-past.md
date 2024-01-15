---
title:                "Oblliczanie daty w przyszłości lub przeszłości"
html_title:           "Go: Oblliczanie daty w przyszłości lub przeszłości"
simple_title:         "Oblliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być przydatne, jeśli na przykład chcesz wyświetlić datę ważnego wydarzenia lub urodzin w programie. Jest to również przydatne dla aplikacji finansowych, które mogą potrzebować obliczeń związanych z datami.

## Jak to zrobić

```Go
package main 
import ( 
    "fmt" 
    "time" 
) 
func main() { 
    // Obliczanie daty w przyszłości 
    fmt.Println("Data za 5 dni:", time.Now().AddDate(0, 0, 5)) 
    // Obliczanie daty w przeszłości 
    fmt.Println("Data sprzed 2 lat:", time.Now().AddDate(-2, 0, 0)) 
} 
```

Wyjście:

```
Data za 5 dni: 2021-07-24 19:34:07.614380113 +0200 CEST 
Data sprzed 2 lat: 2019-07-19 19:34:07.615379945 +0200 CEST 
```

## Głębszy zanurzenie

Pakiet "time" w języku Go zawiera wiele funkcji, które pomagają w manipulowaniu i obliczaniu dat. Metoda "AddDate" pozwala na dodawanie lub odejmowanie określonej liczby lat, miesięcy i dni od bieżącej daty. Jeśli chcesz obliczyć datę w przyszłości lub przeszłości z większą precyzją, możesz użyć metod takich jak "Add", "Sub" lub "AddDate" w połączeniu z pakietem "time.Duration" do obsługi godzin, minut i sekund.

## Zobacz także

- Dokumentacja dla pakietu "time": https://golang.org/pkg/time/
- Przewodnik po języku Go: https://golang.org/doc/
- Inne przydatne linki: https://github.com/avelino/awesome-go