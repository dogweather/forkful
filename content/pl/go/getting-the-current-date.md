---
title:    "Go: Pobieranie aktualnej daty"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Dlaczego warto pobrać bieżącą datę w języku Go?

Pobranie bieżącej daty jest bardzo ważnym aspektem programowania w języku Go. Wiele aplikacji wymaga dostępu do aktualnej daty i godziny, na przykład do tworzenia dzienników zdarzeń lub do zestawień danych. W tym artykule pokażemy Ci, jak w prosty sposób można uzyskać bieżącą datę w języku Go.

## Jak to zrobić?

Aby pobrać aktualną datę w języku Go, należy użyć funkcji `time.Now()`, która zwraca strukturę `Time` zawierającą informacje o bieżącej dacie i godzinie. Poniżej znajduje się prosty kod, który pokazuje to w praktyce:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // pobranie bieżącej daty i godziny
	fmt.Println(currentTime) // wyświetlenie bieżącej daty i godziny
}
```

Output tego programu będzie wyglądał mniej więcej tak:

`2021-08-25 10:30:00.555`

Możemy również sformatować wyjście według swoich preferencji przy pomocy odpowiednich metod struktury `Time`. Na przykład, jeśli chcemy wyświetlić datę w formacie `DD-MM-RRRR`, możemy użyć metody `Format`:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	formattedDate := currentTime.Format("02-01-2006") // formatowanie daty według własnych preferencji
	fmt.Println(formattedDate)
}
```

Output teraz będzie wyglądał tak: `25-08-2021`. Warto także zauważyć, że obszary ozdobne w formacie, takie jak `DD`, `MM` czy `RRRR` muszą być zapisane zgodnie z kolejnością dni, miesięcy i lat, a format musi zawierać cztery cyfry dla roku.

## Pogłębiona analiza

W języku Go dostępne są różne metody i funkcje związane z obsługą dat i godzin. Można na przykład wykonywać operacje na nich, takie jak dodawanie lub odejmowanie określonej liczby lat, miesięcy, dni czy godzin. Zależy to od potrzeb aplikacji, w której jesteś, czy konieczność operowania na dacie i godzinie będzie częstym zadaniem. Wtedy warto zgłębić te kwestie i zapoznać się z dokumentacją języka Go w celu lepszego wykorzystania tej funkcjonalności.

## Zobacz również

- Oficjalna dokumentacja języka Go dotycząca obsługi dat i godzin: https://golang.org/pkg/time/
- Poradnik na temat formatowania dat w języku Go: https://programming.guide/go/format-parse-string-time-date-example.html