---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver innebærer å endre alle store bokstaver i en tekststreng til små bokstaver. Dette utføres ofte for å ignorere forskjeller i store og små bokstaver når man sammenlikner strenger, eller for å normalisere data.

## Hvordan:
Her er et eksempel på hvordan du kan konvertere en streng til små bokstaver i Go:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hei, Verden!"
    lower := strings.ToLower(text)
    fmt.Println(lower)
}
```
Når du kjører denne koden, vil utskriften bli:
```
hei, verden!
```
## Dyp dykk
Ingeniører hos Google utviklet "strings" pakken som håndterer strenger i Go, og det inkluderer funksjonen ToLower. Den har vært en del av språket siden dets opprinnelige utgivelse i 2007. Du kan alternativt bruke "bytes" pakken for å oppnå samme resultat, men det er mer komplisert og ineffektivt for vanlige programmeringsoppgaver. Under panseret i ToLower, bredden av hvert tegn i strengen identifiseres og hvis det er en stor bokstav, konverteres den til en liten bokstav.

## Se Også
Du kan finne mer informasjon om Go og "strings" pakken i følgende ressurser:

1. Go Dokumentasjon: https://golang.org/pkg/strings/
2. Go et effektivt språk for systemprogrammering: https://www.levlup.com/post/go-efficient-language-systems-programming
3. "Strings, bytes, runes and characters in Go": https://blog.golang.org/strings