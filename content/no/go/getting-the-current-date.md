---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:14:31.123826-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hente den gjeldende datoen betyr å finne ut akkurat hva klokken er nå i kode-form. Programmerere trenger dette for å merke tidspunkter, håndtere tid-avhengige funksjoner, og for brukergrensesnitt.

## Hvordan Gjøre:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Gjeldende dato og tid:", currentTime)
}
```
Eksempel på utdata:
```
Gjeldende dato og tid: 2023-03-31 12:04:05.123456 +0200 CEST
```

## Dypdykk
Lenge før `time` pakken kom til Go, var utviklere avhengig av C-biblioteker og systemkall for tidsoperasjoner. Go tilbyr nå `time` pakken som tilbyr et omfattende bibliotek for å håndtere dato og tid. Alternativer inkluderer bruk av tredjepartsbiblioteker som `dateparse` for flere fleksible parsingsalternativer. Ved å bruke `time.Now()`, får vi en `Time` struktur som inneholder dato, tid, og tidssoneinformasjon som kan tilpasses videre med metoder knyttet til `Time` typen.

## Se Også
- Go's offisielle dok for `time` pakken: https://golang.org/pkg/time/
- `dateparse` for mer fleksibel parsing: https://github.com/araddon/dateparse
- Go by Example for praktiske guide på dato og tid: https://gobyexample.com/time