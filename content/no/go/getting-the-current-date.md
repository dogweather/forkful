---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente nåværende dato er prosessen med å hente dagens dato og tidspunkt. Programmere gjør dette for å logge hendelser, sette tidsfrister eller gi realtidsoppdateringer.

## Hvordan:

Her er hvordan du får dagens dato i Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println("Dagensdato og tid er", t)
}
```

Når du kjører det over, bør du få noe sånt som "Dagens dato og tid er 2022-02-22 14:31:07.422636 +0100 CEST m=+0.000456997"

## Dypdykk

Før Go, i eldre språk som C, er det få alternativer for å hente gjeldende dato og klokkeslett, og det var mer komplekst. I Go, skaper 'time' pakken dette enkelt og intuitivt. 'Time.Now()' henter gjeldende dato og klokkeslett fra systemklokken. Andre alternativer inkluderer å bruke time pakken 'Date' metode for å hente dag, måned, og år, eller 'Clock' metode for å hente time, minutt, og sekund.

## Se Også:

1. Go Doc for "time" pakke: https://golang.org/pkg/time/
2. 'Tid og datoer i Go': https://yourbasic.org/golang/format-parse-string-time-date-example/
3. Go Tutorial om dato og tid: https://www.tutorialsteacher.com/golang/golang-datetime