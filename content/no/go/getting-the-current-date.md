---
title:    "Go: Få dagens dato"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen i et programmeringsprosjekt kan virke som en enkel oppgave, men det er likevel viktig for å sikre nøyaktigheten og påliteligheten til applikasjonen din. Det kan også være nyttig for å registrere og spore aktiviteter eller hendelser i programmet ditt, som for eksempel når en fil ble opprettet eller når en transaksjon ble gjort. I denne blogginnlegget skal vi se nærmere på hvordan du kan hente den nåværende datoen i et Go-program.

## Hvordan

Først må du importere «time»-pakken i Go ved å skrive følgende importsetning øverst i filen din:

```Go
import "time"
```

Deretter kan du få den nåværende datoen ved å bruke funksjonen «Now()» fra «time»-pakken, som returnerer en «time.Time»-verdi. Du kan deretter formatere denne verdien ved hjelp av funksjonen «Format()». Se eksempelet nedenfor:

```Go
// Hent nåværende dato og konverter til ønsket format
currentDate := time.Now()
formattedDate := currentDate.Format("02.01.2006") // format: tt.mm.åååå
```
    
Output av denne koden vil være datoen i ønsket format, for eksempel: 13.09.2021.

Det er også mulig å få den nåværende datoen i en annen tidssone ved å bruke funksjonen «FixedZone()». Se eksempelet nedenfor:

```Go
// Hent nåværende dato i en annen tidssone
location, _ := time.LoadLocation("Europe/Oslo")
currentDate := time.Now().In(location)
```

## Dypdykk

Når du bruker funksjonen «Now()» og «LoadLocation()», vil datoen bli hentet fra operativsystemets klokke. Dette kan føre til problemer hvis brukeren endrer klokkeslettet på datamaskinen sin. For å unngå dette, kan du bruke funksjonen «UTC()» som henter datoen fra koordinert universaltid (UTC).

```Go
// Hent nåværende dato fra UTC
currentDate := time.Now().UTC()
```

Det er også verdt å merke seg at «Now()»-funksjonen tar hensyn til tiden i den vertsoperativsystemet, noe som kan føre til avvik hvis serveren din er satt til en annen tidssone enn der du kjører programmet ditt fra. I slike tilfeller er det bedre å bruke funksjonen «Now().UTC()» for å få en mer pålitelig dato.

## Se også

* Offisiell Go-dokumentasjon for Time-pakken: https://golang.org/pkg/time/
* Hvordan håndtere dato og tid i Go: https://golangbyexample.com/go-dato-tid/
* Convert Timezone in Golang: https://www.geeksforgeeks.org/convert-timezone-in-golang/