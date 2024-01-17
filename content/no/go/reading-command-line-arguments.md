---
title:                "Lese kommandolinjeargumenter"
html_title:           "Go: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese kommandolinjeargumenter er en måte for programmerere å ta inn informasjon fra brukeren via terminalen. Dette gjør det mulig å gi programmet ulike instruksjoner og variabler før det kjører. Det er et viktig verktøy for å gjøre programmer mer dynamiske og interaktive.

## Hvordan:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:]
    fmt.Println("Du har gitt", len(args), "kommandolinjeargumenter: ", args)
}
```
Eksempel output: ```Du har gitt 3 kommandolinjeargumenter: [argument1 argument2 argument3]```

## Dykk dypere

Kommandolinjeargumenter har vært en del av programmering siden tidlige datamaskiner, da de ble brukt for å gi programmet instruksjoner på en enkel måte. Alternativene for å lese argumenter i Go inkluderer ```flag``` og ```pflag``` pakker, som gir mer funksjonalitet og fleksibilitet. For å implementere lesing av argumenter i Go, brukes standardbiblioteket "os". 

## Se også

- Offisiell Go dokumentasjon for ```os``` pakken: https://golang.org/pkg/os/
- Go flagg pakken: https://golang.org/pkg/flag/
- Go pflag pakken: https://github.com/spf13/pflag