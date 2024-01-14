---
title:                "Go: Beregning av en dato i fremtiden eller fortiden."
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge eller analysere tidsrelaterte oppgaver. Med Go-programmeringsspråket kan du enkelt lage et program som kan utføre disse beregningene for deg.

## Hvordan

For å beregne en dato i fremtiden eller fortiden, kan du bruke Go sin "time" pakke. Først må du importere denne pakken ved å inkludere "time" i import-setningen.

```Go
import "time"
```

For å beregne en dato i fremtiden, kan du bruke funksjonen "AddDate" og gi den et time.Time-objekt som input, sammen med antall år, måneder og dager du ønsker å legge til. For eksempel, hvis du vil finne datoen 2 år, 6 måneder og 10 dager fra nå, kan du bruke følgende kode:

```Go
now := time.Now()
futureDate := now.AddDate(2, 6, 10)
fmt.Println(futureDate)
```

Dette vil gi følgende output:

```text
2023-04-14 13:45:57.2913995 +0300 +03 m=+95295504.792243019
```

På samme måte kan du beregne en dato i fortiden ved å bruke negative tall som input til "AddDate" funksjonen. For å finne datoen 5 måneder og 15 dager tilbake i tid fra nå, kan du bruke følgende kode:

```Go
now := time.Now()
pastDate := now.AddDate(0, -5, -15)
fmt.Println(pastDate)
```

Dette vil gi følgende output:

```text
2020-10-30 13:46:11.1126119 +0200 CET m=+95295780.613456519
```

## Dykk Dypere

En viktig ting å huske på når du beregner datoer i Go, er at funksjonene for å legge til eller trekke fra datoer tar hensyn til skuddår og korrekt håndtering av datoer som faller utenfor en måned. Dette gjør Go til et pålitelig valg for nøyaktige dato beregninger.

Det er også verdt å merke seg at "AddDate" funksjonen vil returnere en ny time.Time-objekt i stedet for å endre den originale. Dette sikrer at du ikke utilsiktet endrer eller overskriver verdier i dine eksisterende time.Time-objekter.

## Se Også

- [Go time-pakken dokumentasjon](https://golang.org/pkg/time/)
- [Stack Overflow: How to calculate a date in the future or past in Go](https://stackoverflow.com/questions/30254716/how-to-calculate-a-date-in-the-past-or-future-without-leap-years-and-not-timestamp)
- [Medium: Working with dates and times in Go](https://medium.com/@freshwebio/working-with-dates-and-times-in-golang-ccfc82027d7d)