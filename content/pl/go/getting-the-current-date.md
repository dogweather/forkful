---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:14:40.517621-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)

Pobieranie bieżącej daty to uzyskanie informacji o obecnej dacie i czasie z systemu komputerowego. Programiści robią to, by logować zdarzenia, obliczać różnice czasowe lub wyświetlać daty użytkownikom.

## How to (Jak to zrobić):

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Aktualna data i czas:", currentTime)
}

// Przykładowe wyjście:
// Aktualna data i czas: 2023-03-15 11:06:39.123456 +0100 CET
```

## Deep Dive (Dogłębna analiza):

Go (jako młody język stworzony w Google w 2009 r.) oferuje natywne wsparcie dla operacji na datach i czasie za pośrednictwem pakietu `time`. Ta biblioteka dostarcza wiele funkcji, np. `Now()` do pobierania aktualnego czasu. Alternatywą jest użycie zewnętrznych bibliotek, jak `dateparse` dla bardziej złożonych zadań. 

Niemniej, standardowy pakiet `time` zwykle wystarcza. Pozwala na formatowanie czy porównywanie dat, a także operowanie strefami czasowymi. Użycie funkcji `Now()` jest proste, ale pozostaje potężne narzędzie, dzięki możliwości rozszerzenia poprzez metody pakietu `time`, takie jak `Add()` czy `Sub()`.

## See Also (Zobacz również):

- Oficjalna dokumentacja pakietu `time` w Go: https://pkg.go.dev/time
- Artykuł "Understanding Time and Date in Go": https://www.digitalocean.com/community/tutorials/understanding-time-and-date-in-go
- Wprowadzenie do formatowania i parsowania dat w Go: https://gobyexample.com/time-formatting-parsing
