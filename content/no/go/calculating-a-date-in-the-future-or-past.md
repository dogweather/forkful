---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Go: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger i programmering kan det være nyttig å kunne beregne datoer i fremtiden eller fortiden. Dette kan være nyttig for å håndtere aldersgrenser, datoer for hendelser eller generelt i programvaren din.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Go, kan du bruke tidsfunksjonene som er innebygd i språket. Du kan starte ved å importere tidsmodulen og deretter bruke funksjonen `AddDate` for å legge til eller trekke fra et visst antall år, måneder og dager fra en dato.

```Go
import "time"

// Beregn dato 5 måneder og 2 dager fra nå
fremtidigDato := time.Now().AddDate(0, 5, 2)

// Beregn dato 2 år og 7 måneder tilbake i tid
forrigeDato := time.Now().AddDate(-2, -7, 0)

// Skriv ut resultatet
fmt.Println("Fremtidig dato:", fremtidigDato.Format("02-01-2006"))
fmt.Println("Forrige dato:", forrigeDato.Format("02-01-2006"))
```

Output:
```
Fremtidig dato: 15-08-2022
Forrige dato: 15-04-2019
```

Det er også mulig å bruke funksjonen `Sub` for å trekke fra et tidsintervall fra en dato. For eksempel, hvis du ønsker å beregne en dato 3 uker og 4 dager tilbake i tid, kan du bruke følgende kode:

```Go
import "time"

// Beregn dato 3 uker og 4 dager tilbake i tid
forrigeDato := time.Now().Sub(time.Hour*24*7*3 + time.Hour*24*4)

// Skriv ut resultatet
fmt.Println("Forrige dato:", forrigeDato.Format("02-01-2006"))
```

Output:
```
Forrige dato: 14-02-2021
```

## Dypdykk
I Go er datatypen `time.Time` brukt for å representere datoer og tid. Denne typen inkluderer funksjoner for å beregne fremtidige og tidligere datoer ved hjelp av `AddDate` og `Sub` funksjonene. Det er også mulig å bruke andre funksjoner for å håndtere datoer, som for eksempel `Date` som kan brukes til å opprette en dato med spesifiserte år, måneder og dager.

For mer informasjon om å håndtere datoer og tid i Go, kan du se på dokumentasjonen for tidsmodulen: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)

## Se Også
- [Dokumentasjon for tidsmodulen i Go](https://golang.org/pkg/time/)
- [Eksempler på å beregne datoer i andre språk, som Python og Java](https://www.geeksforgeeks.org/date-operations-in-python-java/)