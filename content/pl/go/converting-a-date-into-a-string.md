---
title:                "Konwersja daty na łańcuch znaków"
date:                  2024-01-20T17:37:13.971326-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty do stringa to proces zamiany obiektu reprezentującego datę na tekst. Programiści robią to, by ułatwić wyświetlanie i zapisywanie dat w zrozumiałej formie dla użytkownika i innych systemów.

## Jak to zrobić:
Go używa pakietu `time` do obsługi dat i czasu. Oto prosty sposób na konwersję daty na string:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	fmt.Println("Data w formacie RFC3339:", now.Format(time.RFC3339))
	fmt.Println("Data w polskim formacie:", now.Format("02-01-2006 15:04:05"))
}
```

Przykładowe wyjście:
```
Data w formacie RFC3339: 2023-03-17T14:57:36+01:00
Data w polskim formacie: 17-03-2023 14:57:36
```

## Deep Dive
Konwersja daty do stringa w Go ma korzenie w pakiecie time, wprowadzonym w pierwszej wersji Go. Pakiet time pozwala na wiele formatów, włącznie z predefiniowanymi jak `time.RFC3339`, lub własnymi, dzięki metodzie `Format`. Alternatywą dla `time.Format` może być np. `time.String`, ale zwraca on mniej elastyczne reprezentacje. Implementacje konwersji mogą różnić się zależnie od użytych bibliotek; jednak `time` oferuje jednolity i sprawdzony interfejs.

## Zobacz także
- Dokumentacja pakietu `time`: [golang.org/pkg/time/](https://golang.org/pkg/time/)
- Go by Example: Time Formatting/Parsing: [gobyexample.com/time-formatting-parsing](https://gobyexample.com/time-formatting-parsing)
- Pakiet `time` na Go Playground: [play.golang.org/](https://play.golang.org/) (wystarczy wyszukać przykłady związane z czasem)
