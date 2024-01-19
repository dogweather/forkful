---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å tolke en dato fra en streng i programmering innebærer å konvertere en lesbar datostreng (f.eks. '21-10-2021') til en hensiktsmessig dato datastruktur. Dette gjøres fordi det er mer hensiktsmessig å manipulere datoer som egne datastrukturer enn tekststrenger. 

## Hvordan:

Go har en pakke, `time`, som hjelper oss med dato- og tidsrelaterte funksjoner. Her er et eksempel på bruk:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const shortForm = "2006-01-02"
	str := "2021-10-21"

	t, _ := time.Parse(shortForm, str)
	fmt.Println(t)
}
```

Kjøring av koden ovenfor vil gi oss:

```Go
2021-10-21 00:00:00 +0000 UTC
```

Det vi gjorde her, var å bruke funksjonen `time.Parse`. Den tar strengen som skal analyseres og en "layout" streng som forteller hvordan datoen skal tolkes.

## Dyp Dykk:

Dato tolking er ikke noe nytt og har vært praktisert siden de tidligste dagene av programmering. Go bruker et litt uvanlig system hvor datoen '2. januar 2006 kl. 15:04:05' brukes for å angi formatet. Dette kan virke forvirrende i starten, men er veldig fleksibelt når du blir vant med det.

Alternativt, hvis du behandler datoer som tekststrenger, kan du bli fanget av subtile feil. For eksempel, forskjellige land har forskjellige datoformater. Ved bruk av en dedikert dato datastruktur, unngår man disse problemene.

Implementasjonsdetaljer for `time.Parse` inkluderer å tolke layoutstrengen og matche den mot input strengen. For hver komponent i layoutet, tolker den tilsvarende komponenten i inndatateksten.

## Se Også: 

For mer informasjon, sjekk ut følgende lenker:

1. Go's offisielle `time` pakke dokumentasjon: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
2. Ytterligere forklaring på Go's dato og tidspakke: [https://gobyexample.com/time](https://gobyexample.com/time)
3. En mer detaljert gjennomgang av dato og tidshantering i Go: [https://www.calhoun.io/working-with-dates-and-times-in-go/](https://www.calhoun.io/working-with-dates-and-times-in-go/)