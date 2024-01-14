---
title:                "Go: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan beregne en dato i fremtiden eller fortiden i Go-programmering? Å kunne beregne datoer kan være veldig nyttig når du skal planlegge fremtidige hendelser eller håndtere historiske data. Les videre for å lære mer om dette nyttige konseptet!

## Hvordan

For å beregne en dato i fremtiden eller fortiden i Go, trenger vi å bruke tids- og dato-pakken. Vi kan starte ved å importere pakken og deretter bruke "Now ()" -funksjonen for å få nåværende tid og dato i form av en "tids" -variabel, som vi kan bruke til å generere fremtidige eller fortidige datoer.

```Go
import "tid"
[date] := tid.Nå ()
```

For å beregne en dato i fremtiden, kan vi bruke "AddDate" -funksjonen og gi den antall år, måneder og dager vi ønsker å legge til i nåværende dato. Det samme gjelder for å beregne en dato i fortiden, bare må vi bruke "Sub" -funksjonen i stedet.

```Go
// Beregne dato i fremtiden
fremtidigDato := date.AddDate (år, måned, dag)

// Beregne dato i fortiden
fortidigDato := date.Sub (år, måned, dag)
```

La oss ta en titt på noen eksempler og resultatet de vil gi:

```Go
// Beregne dato i fremtiden: 1 år, 2 måneder og 3 dager
fmt.Println (futureDate) // 2022-08-21

// Beregne dato i fortiden: 1 år, 2 måneder og 3 dager
fmt.Println (pastDate) // 2018-04-17
```

Som du kan se, gir "AddDate" og "Sub" -funksjonene oss nøyaktige datoer basert på antall år, måneder og dager vi ønsker å legge til eller trekke fra.

## Dypdykk

For å gå enda dypere inn i dette emnet, kan vi også bruke "Date" og "Time" -strukturer til å spesifisere en bestemt dato og klokkeslett, og deretter bruke "Add" og "Subtract" -funksjonene for å generere en ny dato basert på denne. Dette gir oss enda større nøyaktighet og kontroll når vi beregner datoer i fremtiden eller fortiden.

Se også:

- [Golang tid og dato pakke dokumentasjon](https://golang.org/pkg/time/)
- [Beregne dato i fremtiden i Go]( https://www.geeksforgeeks.org/how-to-calculate-future-dates-in-golang/)
- [Go-tutorial for å beregne datoer](https://golangbot.com/dates-and-timestamps/)

## Se også

Vi håper denne artikkelen hjalp deg med å lære hvordan du enkelt kan beregne datoer i fremtiden eller fortiden i Go-programmering. Sørg for å sjekke ut lenkene ovenfor for mer detaljert informasjon og eksempler. Lykke til med å bruke dette konseptet i dine egne prosjekter!