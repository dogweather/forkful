---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinjeargumenter innebærer å fange opp og manipulere de innspillene som er gitt i terminalen mens du kjører et program. Programmerere gjør dette for å gi et fleksibelt grensesnitt til applikasjonen.

## Hvordan: 

Skriver vi et enkelt eksempel?

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	arg := os.Args
	fmt.Println(arg)
}
```

Kjører vi dette programmet som `go run main.go hei verden`, vil utskriften være: 

```Go
[main.go hei verden]
```

## Dypdykk

Historisk sett har kommandolinje-argumenter vært et sentralt aspekt av å jobbe med programmeringsspråk. De gir en metode for å sende innspill til et program under kjøretid, noe som åpner for mer dynamisk og interaktiv kode.

Alternativt kan innspill til et Go-program håndteres ved hjelp av flagger. Flaggbiblioteket i Go er ekstremt kraftig og gir større kontroll over det inngående datagrensesnittet.

Et viktig poeng å merke seg om `os.Args` er at første element alltid vil være programnavnet. Det er en implementeringsdetalj som er arvet fra C og Unix, og noe å huske når du behandler argumenter.

## Se Også

For mer detaljer om flaggbiblioteket og avansert argumentbehandling, se [Go doukumentasjonen](https://golang.org/pkg/flag/) og denne [bloggposten om flagger](https://blog.golang.org/flag).