---
title:                "Go: Lage en midlertidig fil"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi utvikler et program, trenger vi midlertidige filer for å lagre data eller utføre spesifikke oppgaver. Det kan være for å sikre at data ikke går tapt, for å lagre mellomresultater eller for å unngå å forurense det eksisterende filsystemet. I denne bloggposten skal vi se på hvordan vi kan lage midlertidige filer i Go-programmeringsspråket.

## Slik gjør du det

Å lage midlertidige filer i Go er ganske enkelt. Det første vi må gjøre er å importere "io/ioutil" pakken som lar oss arbeide med filsystemet. Deretter kan vi bruke "ioutil.TempFile" funksjonen for å lage en midlertidig fil. Her er et eksempel på kode og tilhørende output:

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        panic(err)
    }

    // Skriver til midlertidig fil
    tempFile.WriteString("Dette er en midlertidig fil.")

    // Lukker filen for å frigjøre ressurser
    defer tempFile.Close()

    fmt.Println("Opprettet midlertidig fil:", tempFile.Name())
}
```

Output:

``` 
Opprettet midlertidig fil: /var/folders/78/pyx814nd5_xg400000gn/T/example371412736
```

Etter å ha utført koden over, vil vi se at det er blitt opprettet en midlertidig fil med navnet "example371412736" i mappen "T" på systemet vårt. Vi kan også legge til en tilfeldig streng etter "example" i funksjonen for å skille filene hvis vi for eksempel lager flere på rad.

## Dypdykk

Det er også verdt å merke seg at "ioutil.TempFile" funksjonen også tar inn et valgfritt argument for å angi mappen hvor filen skal opprettes. Hvis dette argumentet ikke er spesifisert, vil en standard mappe bli brukt basert på operativsystemet.

I tillegg er det viktig å huske å lukke den midlertidige filen når vi er ferdig med den, for å frigjøre eventuelle ressurser som den bruker. Dette kan gjøres ved å bruke "defer" nøkkelordet som vil sikre at filen blir lukket selv om det oppstår en feil i koden vår.

## Se også

- [Offisiell dokumentasjon for ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Lag midlertidige filer med ioutil.TempFile i Go](https://www.digitalocean.com/community/tutorials/how-to-create-temporary-files-in-go-using-ioutil-tempfile)