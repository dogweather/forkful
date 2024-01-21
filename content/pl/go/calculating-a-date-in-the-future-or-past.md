---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:31:10.130470-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obliczanie dat przyszłych lub minionych to po prostu znajdowanie daty, która jest określonym odstępem czasu od daty bazowej. Programiści robią to do obsługi ważności, planowania zadań, czy też przewidywania czasu wydarzeń przyszłych.

## Jak to zrobić:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	fmt.Println("Dzisiaj:", today.Format("02-01-2006"))

	nextWeek := today.AddDate(0, 0, 7)
	fmt.Println("Za tydzień:", nextWeek.Format("02-01-2006"))

	lastMonth := today.AddDate(0, -1, 0)
	fmt.Println("Miesiąc temu:", lastMonth.Format("02-01-2006"))
}
```

Przykładowe wyjście:
```
Dzisiaj: 15-04-2023
Za tydzień: 22-04-2023
Miesiąc temu: 15-03-2023
```

## Dogłębna analiza:
Kalkulacje na datach są tak stare, jak konieczność mierzenia czasu. Historia liczenia czasu sięga starożytnego Sumeru. W Go używamy pakietu `time` do operacji na datach. Alternatywnie, można korzystać z bibliotek zewnętrznych jak `dateparse` czy `carbon` dla specyficznych zastosowań. Go już w standardzie oferuje funkcję `AddDate()`, która pozwala na zmianę daty o określoną liczbę lat, miesięcy i dni, a `time.Duration` pozwala na dokładniejsze obliczenia, takie jak godziny, minuty i sekundy.

## Zobacz również:
- Dokumentacja Go na temat pakietu `time`: https://golang.org/pkg/time/
- Biblioteka `dateparse` dla Go: https://github.com/araddon/dateparse
- Biblioteka `carbon` dla Go: https://github.com/golang-module/carbon