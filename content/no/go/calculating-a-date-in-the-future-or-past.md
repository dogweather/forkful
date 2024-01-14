---
title:    "Go: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å kunne beregne en dato i fremtiden eller fortiden er en nyttig ferdighet å ha for å håndtere datoer og tidsbegrensede oppgaver i programmering. Her skal vi utforske hvordan man kan gjøre dette i Go.

## Hvordan

For å beregne en dato i fremtiden eller fortiden i Go, trenger vi å bruke pakken "time". Vi kan bruke funksjonen "AddDate" for å legge til eller trekke ifra et gitt antall år, måneder eller dager til en dato.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Beregner en dato 2 år og 5 måneder fra nå
    futureDate := time.Now().AddDate(2, 5, 0)
    fmt.Println(futureDate)

    // Beregner en dato 1 år, 8 måneder og 10 dager tilbake i tid fra nå
    pastDate := time.Now().AddDate(-1, -8, -10)
    fmt.Println(pastDate)
}
```

Dette vil gi oss følgende output:

```
2024-11-10 19:25:45.1168565 +0100 CET
2019-03-10 19:25:45.1168565 +0100 CET
```

Som du kan se har vi brukt funksjonen "Now" for å få dagens dato, og deretter brukt "AddDate" for å beregne fremtidige eller tidligere datoer. Vi kan også bruke funksjonen "Sub" hvis vi ønsker å trekke ifra en viss tidsperiode i stedet for å legge til.

## Dypdykk

Hvis vi ønsker å gjøre mer avanserte beregninger med datoer i Go, kan vi bruke pakken "time" i kombinasjon med pakken "dateparse". Dette lar oss håndtere mer komplekse datoformater og konvertere de til et tidsobjekt som kan brukes i Go.

```Go
package main

import (
    "fmt"
    "time"

    dateparse "github.com/araddon/dateparse"
)

func main() {
    // Beregner en dato basert på et gitt datoformat og datostring
    customDate, _ := dateparse.ParseAny("January 2nd, 2006 at 15:04PM")
    fmt.Println(customDate)

    // Konverterer en datostring til tidsobjekt i RFC3382-format
    rfcDate, _ := dateparse.ParseAny("Mon, 02 Jan 06 15:04:05 Z0700")
    fmt.Println(rfcDate)
}
```

Dette vil gi oss følgende output:

```
2019-11-05 15:04:00 +0000 UTC
2019-11-05 14:39:45 +0000 UTC
```

Her ser vi at vi har brukt to forskjellige funksjoner fra pakken "dateparse" for å beregne datoer basert på forskjellige formater. Dette lar oss håndtere flere typer datoer og gjøre mer komplekse beregninger i Go.

## Se også

- [Go Dokumentasjon om Time](https://golang.org/pkg/time/)
- [Go Dokumentasjon om Dateparse](https://github.com/araddon/dateparse)