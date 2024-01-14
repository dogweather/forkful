---
title:    "Go: Å gjøre en streng stor bokstav"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Når vi utvikler programmer, kommer vi ofte over situasjoner der vi trenger å manipulere tekststrenger på forskjellige måter. En av disse manipulasjonene kan være å endre strengens første bokstav til stor bokstav. Dette er spesielt nyttig når vi jobber med brukerinput, for å sikre at teksten er riktig formatert. I denne bloggposten vil vi se på hvordan vi kan kapitalisere en streng ved hjelp av Go-programmeringsspråket.

## Hvordan

For å kapitalisere en streng i Go, kan vi bruke funksjonen "strings.Title()" fra standardbiblioteket. La oss ta en titt på et eksempel nedenfor:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "dette er en setning"

    // Kapitaliser første bokstav i strengen
    kapitalisertStr := strings.Title(str)
    
    fmt.Println(kapitalisertStr) // Output: Dette Er En Setning
}
```

Som du kan se, bruker vi funksjonen "strings.Title()" på den opprinnelige strengen, og den returnerer en ny streng med den første bokstaven kapitalisert. Det er også viktig å merke seg at denne funksjonen bare kapitaliserer den første bokstaven, og ikke resten av strengen.

## Dypdykk

Hvis du er interessert i å lære mer om hvordan funksjonen "strings.Title()" fungerer, kan vi se litt nærmere på kildekoden til denne funksjonen. Dette vil gi oss en dypere forståelse av hvordan strengmanipulering fungerer i Go. Her er kildekoden for funksjonen fra standardbiblioteket:

```Go
func Title(s string) string {
    return Fprintf(&b, "%s", s)
}
```

Som du kan se, bruker funksjonen "Fprintf()" til å kapitalisere den første bokstaven i strengen. Hvis du vil utforske kildekoden for "Fprintf()", kan du gjøre det ved å følge lenken nedenfor.

## Se også

- "strings" pakken i Go standardbiblioteket: https://golang.org/pkg/strings/
- Kildekoden for "strings.Title()" funksjonen: https://golang.org/src/strings/strings.go?s=11858:11908#L426
- Kildekoden for "Fprintf()" funksjonen: https://golang.org/src/fmt/print.go?s=21603:21645#L685