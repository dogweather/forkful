---
title:                "Go: Konvertere en streng til små bokstaver"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig i mange tilfeller, spesielt når det kommer til formatering og sammenligning av tekster. Det er også en vanlig oppgave som må håndteres i mange programmeringsspråk, inkludert Go.

## Hvordan

I Go kan du enkelt konvertere en streng til små bokstaver ved å bruke metoden "ToLower" fra "strings" pakken. Her er et enkelt eksempel på hvordan du kan gjøre det:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HEI, JEG ER EN STRENG"
	lowercase := strings.ToLower(str)
	fmt.Println(lowercase)
}

// Output: hei, jeg er en streng
```

Som du kan se, bruker vi "ToLower" metoden på en streng og lagrer resultatet i en ny variabel. Deretter skriver vi ut den nye strengen som er i små bokstaver. Her er noen viktige ting å merke seg om denne metoden:

- Den returnerer en ny streng i små bokstaver, originalstrengen forblir uendret.
- Hvis du prøver å bruke denne metoden på en tom streng, vil den returnere en tom streng.
- Den støtter også andre språk enn engelsk, så dersom du har en streng med spesialtegn eller andre tegn fra andre språk, vil den også bli konvertert til små bokstaver.

## Dypdykk

Under overflaten bruker "ToLower" metoden en teknikk som kalles Unicode normalisering. Dette er en måte å sørge for at ulike tegn i tekster har en konsekvent representasjon, uavhengig av hvilket språk eller tegnsett de tilhører. Dette kan forhindre mulige problemer med sammenligning eller bokstavrekkefølge.

En annen viktig ting å merke seg er at konverteringen til små bokstaver kan variere avhengig av hvilket språk og tegnsett som er i bruk. For eksempel vil ß (tysk stor bokstav s) i de fleste tilfeller bli konvertert til ss, mens den kinesiske karakteren 降 vil bli konvertert til dens små bokstavvariant 霜.

## Se også

Nedenfor er noen nyttige ressurser for å lære mer om konvertering av strenger til små bokstaver i Go:

- [Offisiell dokumentasjon for "strings" pakken i Go](https://golang.org/pkg/strings/)
- [En god artikkel om Unicode normalisering og strenger i Go](https://blog.golang.org/normalization)
- [En video som forklarer Go sin "strings" pakke og hvordan du håndterer tekst](https://www.youtube.com/watch?v=sJVrNWc-7Xk)