---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Go: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Dato-beregning lar oss finne fremtidige eller tidligere datoer fra en gitt dato. Dette er nyttig for programvareutviklere når de må håndtere oppgaver relatert til tidsplanlegging, for eksempel påminnelser, friststyring og tidsavhengige beregninger. 

## Hvordan:
Her er hvordan du beregner en fremtidig dato i Go:

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    today := time.Now()
    forsteJanuar := time.Date(today.Year(), time.January, 1, 0, 0, 0, 0, today.Location())
    enMånedFrem := forsteJanuar.AddDate(0, 1, 0)
    
    fmt.Println(enMånedFrem)
}
```
Dette programmet vil skrive ut datoen for 1. februar i det nåværende året.

## Dykker dypere
- #### Historisk sammenheng:
I eldre programmeringsspråk, måtte dato-beregning vanligvis håndteres manuelt med lavnivå dato- og tid-operasjoner. Dette kunne lett føre til feil og var vanskelig å vedlikeholde.

- #### Alternativer:
Selv om `AddDate` fungerer bra for de fleste tilfeller, kan `Add` også brukes til å legge til tid med enda mer presisjon. 
```Go
enTimeOgTrettiMinFrem := today.Add(time.Hour + 30*time.Minute)
```
- #### Implementasjonsdetaljer:
`AddDate` metoden i Go tar 3 parametere: antall år, antall måneder og antall dager du vil endre datoen med. Den håndterer også automatisk skuddår, forskjellige månedslengder og tidssoneforskjeller.
  
## Se også
- GoDocs om tidspakken: https://golang.org/pkg/time/
- Blogginnlegg - Goint faller i tid : https://blog.golang.org/playground
- Go tidsformatering og parsing: https://gobyexample.com/time-formatting-parsing

Ingen konklusjon kreves i dette innlegget. Lykke til med din Go programmering!