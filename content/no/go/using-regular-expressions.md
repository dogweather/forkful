---
title:    "Go: Å bruke regulære uttrykk"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Når du arbeider med Go-programmering, kan det hende du kommer over utfordrende oppgaver som krever behandling av tekstdata. I slike tilfeller kan det være lurt å bruke regulære uttrykk. Disse uttrykkene gjør det enklere å finne og manipulere tekst på en mer effektiv måte.

## Hvordan

For å bruke regulære uttrykk i Go, må du først importere pakken "regexp". Du kan deretter bruke funksjonen "MatchString" til å finne en bestemt streng i en tekst. La oss si at du ønsker å finne alle ord som starter med "go" i en tekst. Du kan bruke følgende kode:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Opprett et regulært uttrykk som matcher ord som starter med "go"
    regex := regexp.MustCompile(`go\w+`)

    // Definer en tekst som skal undersøkes
    tekst := "Jeg liker å programmere i Go, det er så gøy!"

    // Bruk funksjonen MatchString for å finne alle matches og lagre det i et array
    result := regex.FindAllString(tekst, -1)

    // Iterer gjennom arrayet og skriv ut resultatet
    for _, s := range result {
        fmt.Println(s)
    }
}
```

Output:
```Go
go, Go, gøy
```

## Dypdykk

Det finnes flere metoder i pakken "regexp" som du kan bruke for å gjøre det enklere å behandle tekstdata. Her er noen av dem:

- `Match`: Brukes for å finne matchende strenger og returnerer en boolsk verdi avhengig av om det ble en match eller ikke.
- `ReplaceAllString`: Brukes for å erstatte matchende strenger med en annen tekst.
- `Split`: Brukes for å dele en tekst i et array basert på et gitt regulært uttrykk.

Det finnes også flere regulære uttrykkstyper du kan bruke for å finne mer spesifikk tekst, som for eksempel tall, bokstaver, eller spesifikke mønstre.

## Se Også

- Regular expressions i Go-dokumentasjonen: https://golang.org/pkg/regexp/
- Regulære uttrykk cheat sheet: https://www.debuggex.com/cheatsheet/regex/go
- Video tutorial om bruk av regulære uttrykk i Go: https://www.youtube.com/watch?v=Y6ibmYa6cgc