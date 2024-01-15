---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Go: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których programista musi przekształcić datę na ciąg znaków w języku Go. Na przykład może to być potrzebne do wyświetlenia daty w czytelny sposób dla użytkownika lub do zapisania jej w pliku lub bazie danych.

## Jak to zrobić

Aby przekonwertować datę na string w języku Go, możemy wykorzystać funkcję "Format" z pakietu "time". Oto przykładowy kod:

```Go
package main
import (
    "fmt"
    "time"
)
func main() {
    // Pobranie aktualnej daty i czasu
    now := time.Now()
    // Ustawienie formatu wyświetlania daty i czasu
    format := "2006-01-02 15:04:05"
    // Użycie funkcji Format do przekonwertowania daty na string w wybranym formacie
    dateToString := now.Format(format)
    // Wyświetlenie wyniku
    fmt.Println(dateToString)
}
```

Przykładowy wynik wyświetlania daty i czasu w formacie YYYY-MM-DD HH:MM:SS:

`2020-10-30 08:30:00`

## Deep Dive

Funkcja "Format" z pakietu "time" w języku Go pozwala na precyzyjne ustawienie formatu wyświetlania daty i czasu. W powyższym przykładzie wykorzystaliśmy format "2006-01-02 15:04:05", który jest standardowym formatem dla daty i czasu w języku Go. Ale istnieje wiele innych opcji, np.:

- `YYYY-MM-DD` - wyświetla datę w formacie rok-miesiąc-dzień
- `HH:MM:SS` - wyświetla czas w formacie godzina:minuta:sekunda
- `Jan 02, 2006` - wyświetla datę w formacie miesiąc dzień, rok

Możemy także dodawać dodatkowe elementy, takie jak nazwy dni tygodnia (np. `Mon`, `Tue`), numery miesięcy (np. `01`, `02`) czy też różne znaki oddzielające elementy daty i czasu (np. `.` , `/`).

## Zobacz także

- Dokumentacja pakietu "time" w języku Go: https://golang.org/pkg/time/
- Przykładowe formaty daty i czasu w języku Go: https://yourbasic.org/golang/format-parse-string-time-date-example/