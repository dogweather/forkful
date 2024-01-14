---
title:    "Go: Store bokstaver i en streng"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kode i Go kan være både givende og utfordrende. Noen ganger er det de enkleste funksjonene som kan være de mest nyttige, som for eksempel å kapitalisere en tekststreng. Ved å gjøre dette kan du få en mer lesbar og strukturert utskrift, noe som kan være spesielt nyttig når du jobber med store mengder data. 

## Hvordan

For å kapitalisere en tekststreng i Go, kan du bruke funksjonen "strings.ToUpper" fra standardbiblioteket. Denne funksjonen tar inn en tekststreng som argument og returnerer en versjon av teksten med bare store bokstaver. La oss se på et eksempel:

```Go 
package main

import (
    "fmt"
    "strings"
)

func main() {
    tekst := "dette er en test"
    fmt.Println("Original tekst:", tekst)
    fmt.Println("Kapitalisert tekst:", strings.ToUpper(tekst))
}
```
Dette vil gi følgende utskrift: 
```
Original tekst: dette er en test
Kapitalisert tekst: DETTE ER EN TEST
```
Som du kan se, har teksten blitt konvertert fra små til store bokstaver.

## Dypdykk

En interessant ting å merke seg er at funksjonen "strings.ToUpper" bruker unicode-tabellen for å bestemme hvilke bokstaver som skal konverteres til store bokstaver. Dette betyr at det også vil fungere for bokstaver fra andre alfabet, som for eksempel det norske alfabetet med æ, ø og å. 

Det er også verdt å nevne at Go inneholder flere innebygde funksjoner for manipulering av tekst, som for eksempel "strings.ToLower" for å konvertere til små bokstaver, og "strings.Title" for å gjøre første bokstav i hvert ord stor. Utforsk disse og andre funksjoner for å få enda mer ut av tekstbehandlingen i Go. 

## Se også

- Offisiell dokumentasjon for "strings.ToUpper": https://golang.org/pkg/strings/#ToUpper
- Ekstra funksjoner for tekstbehandling i Go: https://golang.org/pkg/strings/