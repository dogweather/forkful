---
title:    "Go: Konvertere en streng til små bokstaver"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til enkle bokstaver er en grunnleggende funksjon i mange programmeringsspråk, inkludert Go. Det er nyttig for å unngå sammenligningsfeil ved å sikre at to strenger med samme tekst, men med forskjellige bokstaver (store eller små), blir ansett som like. 

## Hvordan

Konverteringen av en streng til små bokstaver i Go kan gjøres ved hjelp av den innebygde funksjonen `strings.ToLower()`. Funksjonen tar en streng som argument og returnerer en ny streng med samme innhold, men alle bokstavene er konvertert til små bokstaver. 

```Go
package main 

import ( 
	"fmt"
	"strings"
)

func main() {
	str := "HEI ALLE SAMMEN"
	lowerCaseStr := strings.ToLower(str)
	fmt.Println(lowerCaseStr) // utskrift: hei alle sammen
}
```

## Dypdykk

Det er viktig å merke seg at konverteringen til små bokstaver følger Unicode-prinsippene. Dette betyr at den håndterer ikke-vestlige tegn korrekt, som for eksempel akutt aksent (á) som blir konvertert til vanlig a (a). Det er også viktig å merke seg at konverteringsfunksjonen tar hensyn til lokale språkinnstillinger. Dette kan føre til at noen bokstaver, som for eksempel tyske umlaut (ä, ö, ü), behandles annerledes enn i andre programmeringsspråk. 

## Se også

- Dokumentasjon for `strings.ToLower()` funksjonen: https://golang.org/pkg/strings/#ToLower
- Informasjon om Unicode-prinsipper i Go: https://blog.golang.org/strings
- Diskusjon om lokale språkinnstillinger og konvertering av bokstaver: https://stackoverflow.com/questions/7059968/how-do-i-convert-a-string-to-another-locale-in-go