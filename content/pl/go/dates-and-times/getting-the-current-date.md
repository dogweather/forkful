---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:03.629527-07:00
description: "Pobieranie bie\u017C\u0105cej daty w j\u0119zyku Go to podstawowe zadanie\
  \ dla programist\xF3w, por\xF3wnywalne z powszechno\u015Bci\u0105 \u201EHello, World!\u201D\
  . Jest niezb\u0119dne do zada\u0144 takich\u2026"
lastmod: '2024-03-13T22:44:34.864027-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w j\u0119zyku Go to podstawowe zadanie\
  \ dla programist\xF3w, por\xF3wnywalne z powszechno\u015Bci\u0105 \u201EHello, World!\u201D\
  . Jest niezb\u0119dne do zada\u0144 takich\u2026"
title: "Pobieranie bie\u017C\u0105cej daty"
weight: 29
---

## Co i dlaczego?

Pobieranie bieżącej daty w języku Go to podstawowe zadanie dla programistów, porównywalne z powszechnością „Hello, World!”. Jest niezbędne do zadań takich jak logowanie i znakowanie czasem wydarzeń, obliczanie czasu trwania i planowanie przyszłych wydarzeń.

## Jak to zrobić:

W Go pakiet `time` jest Twoją bramą do pracy z datami i czasem. Funkcja `time.Now()` daje ci aktualną datę i czas, podczas gdy inne funkcje i metody pozwalają formatować lub manipulować tymi danymi. Oto jak uzyskać bieżącą datę i jej różne reprezentacje:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // Pobiera bieżącą datę i czas
	fmt.Println("Aktualny czas:", currentTime)

	// Aby uzyskać datę w formacie RRRR-MM-DD
	fmt.Println("Aktualna data:", currentTime.Format("2006-01-02"))

	// Aby uzyskać poszczególne składniki daty
	year, month, day := currentTime.Date()
	fmt.Printf("Rok: %d, Miesiąc: %s, Dzień: %d\n", year, month, day)

	// Aby uzyskać dzień tygodnia
	fmt.Println("Dzień tygodnia:", currentTime.Weekday())
}
```

Przykładowy wynik może wyglądać tak:

```
Aktualny czas: 2023-04-18 15:04:05.123456 +0000 UTC
Aktualna data: 2023-04-18
Rok: 2023, Miesiąc: April, Dzień: 18
Dzień tygodnia: Tuesday
```

Zwróć uwagę, jak `Format` używa konkretnej daty (2006-01-02) jako ciągu formatującego. Jest to wybrana przez Go data odniesienia, służąca jako wzorzec mnemotechniczny do formatowania dat.

## Dokładniejsze spojrzenie

Decyzja o użyciu pakietu `time` do manipulacji datą i czasem w Go odzwierciedla zaangażowanie języka w solidne i intuicyjne biblioteki standardowe. W przeciwieństwie do niektórych języków, które mogą mieć wiele konkurujących bibliotek lub metodologii do manipulacji datami, Go stawia na posiadanie jednej, dobrze udokumentowanej normy.

Niecodzienny wybór daty odniesienia (`Mon Jan 2 15:04:05 MST 2006`) w formatowaniu czasu w Go, choć początkowo może być mylący, jest tak naprawdę mistrzowskim posunięciem pod względem użyteczności. Pozwala to programistom przedstawiać formaty daty i czasu, korzystając z podejścia opartego na przykładach, w przeciwieństwie do zapamiętywania tokenów lub symboli, których mogą używać inne języki.

Mimo że pakiet `time` oferuje wszechstronną funkcjonalność dla większości potrzeb, obsługa stref czasowych i zmian DST (czasu letniego) czasami może sprawić trudność nowym programistom Go. Zrozumienie, jak Go obsługuje czas związany z lokalizacją, jest kluczowe, aby uniknąć typowych pułapek w manipulowaniu czasem.

Dla bardziej złożonych potrzeb planowania czy manipulacji czasem, biblioteki stron trzecich, takie jak `github.com/robfig/cron` dla Go, mogą oferować bardziej specjalistyczną funkcjonalność niż standardowy pakiet `time`. Jednak dla większości zastosowań wymagających uzyskania i obsługi bieżącej daty i czasu, pakiet `time` oferuje solidny i idiomatyczny punkt wyjścia w Go.
