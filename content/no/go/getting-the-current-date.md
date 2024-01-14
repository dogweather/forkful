---
title:                "Go: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Å få den nåværende datoen er en vanlig oppgave i programmering. Det er nyttig for å sjekke når en bestemt handling er utført, eller for å lage skreddersydde beskjeder og meldinger basert på datoen. I denne bloggposten vil vi utforske hvordan du kan få den aktuelle datoen ved hjelp av Go-programmeringsspråket.

# Hvordan

For å få den nåværende datoen i Go, kan du bruke innebygde funksjoner som `Time.Now()` og `Format()`.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Få den nåværende datoen
    now := time.Now()

    // Konverter datoen til ønsket format
    formattedDate := now.Format("02 January 2006")

    // Skriv ut datoen
    fmt.Println(formattedDate)
}
```

Dette vil skrive ut datoen på formatet "dd. måned år", for eksempel "06 September 2021".

# Dykk dypere

Go's `Time`-pakke tilbyr en rekke nyttige funksjoner for håndtering av dato og tid. For å få mer nøyaktige tidsverdier, kan du bruke funksjoner som `Now()` og `Unix()`. Du kan også endre tidssone ved å bruke `LoadLocation()` og `In()`-funksjoner.

En annen nyttig funksjon er `Parse()`, som lar deg konvertere en streng til en tidsverdi. Dette kan være nyttig når du jobber med brukerinput eller når du trenger å konvertere en dato fra et annet format.

# Se også

- [Offisiell Go-dokumentasjon for Time-pakken](https://golang.org/pkg/time/)
- [En guide for å håndtere dato og tid i Go](https://www.calhoun.io/working-with-dates-and-time-in-go/)
- [Presentasjon om håndtering av tid i Go](https://talks.golang.org/2015/gotham-dates.slide#1)