---
title:                "Få dagens dato"
html_title:           "Go: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

I vår daglige bruk av datamaskiner og mobilenheter er det ofte nødvendig å vise den aktuelle datoen. Derfor er en av de vanligste operasjonene i programmering å få tak i den aktuelle datoen. Gjennom å bruke Go programming language, kan du enkelt få tak i og manipulere datoen i dine programmer.

## Hvordan

Det første du må gjøre for å få den aktuelle datoen i Go, er å importere "time" pakken. Deretter kan du bruke "Now()" funksjonen for å få tak i nåværende tid og dato. Her er et eksempel på hvordan du kan gjøre dette i Go:

```Go
import "time"

now := time.Now()
fmt.Println("Den aktuelle datoen er:", now)
```

Det vil skrive ut den nåværende dato og tidspunkt basert på tolleranseens format.

```sh
Den aktuelle datoen er: 2021-06-25 14:30:00 +0000 UTC m=+0.000000000
```

Du kan også bruke "Format()" funksjonen for å få datoen i et bestemt format. Her er et eksempel på hvordan du kan få datoen i et format basert på dag, måned og år:

```Go
time.Now().Format("02 Jan 06")
```

Det vil skrive ut datoen som "25 Jun 21", hvor 02 står for dag, Jan står for måned og 06 står for året.

## Dykk Dypere

I tillegg til å få tak i den aktuelle datoen, kan du også manipulere datoen i dine programmer ved hjelp av Go. Du kan legge til eller trekke fra tid fra den nåværende datoen ved hjelp av "Add()" funksjonen. Du kan også sjekke om et gitt år er et skuddår eller ikke, ved hjelp av "IsLeap()" funksjonen.

Her er noen flere ressurser for å hjelpe deg med Go og datoen:

- Offisiell Go Dokumentasjon: https://golang.org/pkg/time/
- Go Playground: https://play.golang.org/
- YouTube Tutorials: https://www.youtube.com/results?search_query=go+date

## Se også

- Go Offisiell Hjemmeside: https://golang.org/
- Go Code Eksempler: https://github.com/golang/go/wiki/Projects