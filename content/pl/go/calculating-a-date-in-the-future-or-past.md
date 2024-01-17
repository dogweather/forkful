---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Go: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Obliczanie daty w przeszłości lub przyszłości jest procesem, w którym programista wykorzystuje język programowania Go do określenia daty, która jest pewną liczbą dni wcześniej lub później niż aktualna data. Jest to przydatne w wielu aplikacjach, takich jak tworzenie kalendarza, wyświetlanie daty ważnych wydarzeń lub obliczanie wieku.

## Jak to zrobić:
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // obecna data
    today := time.Now()

    // ustalenie przyszłej daty o 5 dni
    futureDate := today.AddDate(0, 0, 5)

    // wyświetlenie daty w formacie dnia-miesiąc-rok
    fmt.Println(futureDate.Format("02-01-2006"))
}
```

Output: ```18-09-2021```

## Głębsze wgląd:
### Kontekst historyczny:
Obliczanie daty w przeszłości lub przyszłości jest przydatne od początków programowania, kiedy to ludzie zaczęli wykorzystywać komputery do przechowywania i przetwarzania danych.

### Alternatywy:
Istnieje wiele innych języków programowania, które również pozwalają na obliczanie dat w przeszłości lub przyszłości. Na przykład w języku Python można wykorzystać moduł ```datetime``` do tego celu.

### Szczegóły implementacji:
W języku Go funkcja ```time.AddDate()``` przyjmuje trzy parametry: rok, miesiąc oraz dzień i zwraca wynik w postaci daty. Dodatkowo, funkcja ```Format()``` pozwala na określenie żądanego formatu wyświetlanej daty.

## Zobacz także:
- Dokumentacja języka Go na temat funkcji ```time.AddDate()```: https://golang.org/pkg/time/#Time.AddDate
- Przykład obliczania daty w przeszłości w języku Python: https://python101.readthedocs.io/pl/latest.asm/functions/time.html