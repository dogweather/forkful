---
title:    "Go: Sammenligner to datoer"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave innen programmering, spesielt når man jobber med tidspunkter og datofunksjoner. Å forstå hvordan man kan sammenligne to datoer vil hjelpe deg med å lage mer presis og pålitelig kode. I denne bloggposten vil vi ta en titt på hvordan du kan gjøre dette ved hjelp av Go-programmeringsspråket.

## Hvordan

En enkel måte å sammenligne to datoer på i Go er å bruke standardbiblioteket "time". La oss anta at du har to datoer, "dato1" og "dato2", og du ønsker å finne ut om dato1 kommer før eller etter dato2. I koden nedenfor kan du se dette eksempelet i aksjon:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    dato1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    dato2 := time.Date(2021, time.January, 2, 0, 0, 0, 0, time.UTC)

    if dato1.Before(dato2) {
        fmt.Println("dato1 kommer før dato2")
    } else if dato2.Before(dato1) {
        fmt.Println("dato1 kommer etter dato2")
    } else {
        fmt.Println("datoene er like")
    }
}
```

Dette koden vil gi følgende output:

```
dato1 kommer før dato2
```

Her bruker vi metoden "Before" fra "time" biblioteket for å sammenligne de to datoene. Vi kan også bruke metoden "After" for å gjøre den motsatte sammenligningen. Om begge datoene er like, vil vi få utskriften "datoene er like".

En annen nyttig metode for å sammenligne datoer er "Equal". Denne metoden sammenligner datoer uten å ta hensyn til tidssone og nøyaktighet til millisekunder. Her er et eksempel på hvordan du kan bruke denne metoden:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    dato1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    dato2 := time.Date(2021, time.January, 1, 0, 0, 0, 500, time.UTC)

    if dato1.Equal(dato2) {
        fmt.Println("datoene er like")
    } else {
        fmt.Println("datoene er ikke helt like")
    }
}
```

Dette vil gi følgende output:

```
datoene er like
```

## Dypdykk

Å sammenligne datoer kan være mer komplisert enn bare å bruke de nevnte metodene. Det er viktig å ta hensyn til forskjellige tidssoner, nøyaktighet og også forskjellige kalendere. Go har en god støtte for å jobbe med datoer og kan hjelpe deg med å håndtere disse utfordringene.

Noen nyttige funksjoner for å jobbe med datoer i Go er "Parse", "Format" og "Sub". Disse funksjonene lar deg konvertere datoer fra og til forskjellige formater og også substrahere eller legge til tid.

Se også

* [Offisiell Go dokumentasjon om "time" biblioteket](https://golang.org/pkg/time/)
* [Enkel guide til å jobbe med datoer i Go](https://medium.com/learning-the-go-programming-language/working-with-dates-in-go-72cacc602d68)
* [Eksempelkode for dato/klokkeslett manipulasjon i Go](https://github.com/golang/example/blob/master/time/time.go)