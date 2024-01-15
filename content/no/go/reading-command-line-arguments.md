---
title:                "Å lese kommandolinjeargumenter"
html_title:           "Go: Å lese kommandolinjeargumenter"
simple_title:         "Å lese kommandolinjeargumenter"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer kanskje på hvorfor noen ville være interessert i å lese kommandolinjeargumenter. Vel, å kunne lese og tolke disse argumentene er en viktig del av programmeringsspråket Go. Det gir deg muligheten til å lage mer dynamiske og tilpasningsdyktige programmer.

## Hvordan

For å lese kommandolinjeargumenter i Go, bruker vi pakken "os". Her er et eksempel på hvordan du kan lese et enkelt argument fra terminalen:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  argument := os.Args[1]
  fmt.Printf("Du skrev inn: %s", argument)
}
```

Dette eksempelet tar det første argumentet som brukeren skriver inn i kommandolinjen og skriver det ut. Hvis brukeren skrev for eksempel "go run main.go hello", ville utskriften bli "Du skrev inn: hello".

Du kan lese flere argumenter ved å bruke en løkke:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  arguments := os.Args[1:]

  for _, argument := range arguments {
    fmt.Printf("%s ", argument) // Legger til et mellomrom mellom hvert argument
  }
}
```

Nå vil utskriften bli alle argumentene som er skrevet inn, separert med mellomrom.

## Dypdykk

Det er også mulig å lese flagg (flags) fra kommandolinjen ved hjelp av pakken "flag". Dette gir deg muligheten til å lese spesifikk informasjon fra brukeren, for eksempel om de ønsker å bruke en bestemt funksjonalitet i programmet. Her er et eksempel på hvordan du kan lese et flagg:

```Go
package main

import (
  "flag"
  "fmt"
)

func main() {
  name := flag.String("navn", "", "Ditt navn")
  age := flag.Int("alder", 0, "Din alder")

  flag.Parse()
  fmt.Printf("Hei, %s. Du er %d år gammel!", *name, *age)
}
```

Hvis du kjører programmet med flaggene "-navn=John" og "-alder=25", vil utskriften bli "Hei, John. Du er 25 år gammel!"

## Se Også

- Offisiell dokumentasjon om å lese kommandolinjeargumenter i Go: https://golang.org/pkg/os/#Args
- Ekstra informasjon om å lese flagg: https://golang.org/pkg/flag/
- En tutorial om å lese kommandolinjeargumenter i Go: https://gobyexample.com/command-line-arguments