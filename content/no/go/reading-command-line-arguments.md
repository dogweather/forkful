---
title:    "Go: Lesing av kommandolinje-argumenter"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av kommandolinje argumenter er en viktig og grunnleggende del av å skrive effektiv Go-kode. Uten å kunne lese argumentene som brukes til å kjøre et program, kan programmet vårt ikke tilpasses eller håndtere ulike situasjoner. Derfor er det viktig å forstå hvordan man leser disse argumentene for å skrive robuste programmer.

## Slik gjør du det

Vi kan lese kommandolinje argumenter ved å bruke "os" pakken i Go. Først må vi importere denne pakken øverst i vårt Go-program:

```Go
import "os"
```

Deretter kan vi bruke funksjonen "os.Args" for å få en liste over alle argumentene som ble brukt til å kjøre programmet vårt:

```Go 
args := os.Args
```

Vi kan også få tilgang til hvert enkelt argument basert på indeksen den ble brukt på kommandolinjen. For eksempel, hvis vi har et program som heter "program.go" og kjører det med argumentet "hello", kan vi aksessere dette argumentet på følgende måte:

```Go 
arg := os.Args[1] // Vil returnere "hello"
```

## Dykk dypere

Det er også mulig å lese og håndtere kommandolinje flagg, som er spesielle argumenter som starter med en "-". Dette gjøres ved å bruke "flag" pakken i Go. For å lese et flagg, må vi først definere det ved hjelp av "flag" pakken og deretter bruke funksjonen "flag.Parse()" for å kunne lese det fra kommandolinjen.

For eksempel, hvis vi vil ha et flagg kalt "output" som lar brukeren spesifisere navnet på en fil som programmet skal skrive ut til, kan vi definere det på følgende måte:

```Go 
output := flag.String("output", "", "Navnet på utskriftsfilen")
```

Vi kan da lese verdien av dette flagget i programmet vårt ved å bruke variabelen som ble definert:

```Go 
outputFile := *output
```

For å kjøre programmet vårt med dette flagget, ville vi bruke følgende kommando:

```bash
go run program.go -output=output.txt
```

## Se også

For mer informasjon og eksempler på hvordan man leser kommandolinje argumenter i Go, kan du ta en titt på følgende ressurser:

- [Offisiell dokumentasjon for "os" pakken](https://golang.org/pkg/os/#pkg-examples)
- [Offisiell dokumentasjon for "flag" pakken](https://golang.org/pkg/flag/)
- [Artikkel om å lese kommandolinje argumenter i Go av Todd McLeod](https://medium.com/@toddmcLeod/reading-command-line-arguments-in-go-7d7ef7925ba1)