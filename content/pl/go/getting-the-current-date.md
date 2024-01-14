---
title:                "Go: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego warto pobrać bieżącą datę w programowaniu Go? Ponieważ jest to niezwykle ważna funkcja do wykonywania wielu zadań, takich jak śledzenie czasu, tworzenie raportów lub tworzenie harmonogramów działania programów.

## Jak to zrobić?

## _Kod na czas_

```Go
import "fmt"
import "time"

func main() {
    // Pobranie bieżącej daty
    currentDate := time.Now()
    
    // Wyświetlenie bieżącej daty w określonym formacie
    fmt.Println("Bieżąca data:", currentDate.Format("2006-01-02"))
}
```
**Output:**

```
Bieżąca data: 2019-10-22
```

Możemy również pobrać tylko bieżący rok, miesiąc lub dzień z bieżącej daty za pomocą odpowiednich metod z biblioteki `time`.

## Deep Dive

Możemy odkryć głębiej jak funkcja `Now()` z biblioteki `time` działa w celu pobrania bieżącej daty. Ta funkcja wywołuje systemowe API w celu pobrania aktualnego czasu. Program Go korzysta z biblioteki C do wywoływania odpowiedniej funkcji systemowej, co zapewnia dokładność pobrania daty.

## Zobacz również

- Oficjalna dokumentacja biblioteki `time`: https://golang.org/pkg/time/
- Przykłady wykorzystania pobierania bieżącej daty w programowaniu Go: https://www.golangprograms.com/how-to-get-current-date-time-in-golang.html 
- Poradnik na temat manipulacji datami i czasem w Go: https://yourbasic.org/golang/format-parse-string-time-date-example/