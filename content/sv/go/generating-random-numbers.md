---
title:                "Go: Generering av slumpmässiga nummer"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Slumpmässiga nummer är ett viktigt koncept inom programmering och kan användas för att skapa dynamiska och varierande resultat i koden. Genom att generera slumpmässiga nummer kan man till exempel skapa spel, simulationsprogram och mycket mer.

## Så här gör du
För att generera slumpmässiga nummer i Go använder man sig av paketet "math/rand". Först måste man importera paketet och sedan initialisera en randgenerator med hjälp av en seed-funktion. Detta görs enligt följande kod:

```Go
import "math/rand"

// Initialiserar randgenerator med en seed baserad på aktuell tid
rand.Seed(time.Now().UnixNano())

// Generera slumpmässigt heltal mellan 1-10
randomNumber := rand.Intn(10) + 1

// Generera slumpmässigt decimaltal mellan 0.0-1.0
randomFloat := rand.Float64() 
```

Dessa kodblock visar hur man kan generera slumpmässiga heltal och decimaltal med hjälp av randgeneratorn. Det finns också andra funktioner för att generera mer specifika typer av slumpmässiga värden, som till exempel slumpmässiga booleska värden eller slumpmässiga tecken.

## Djupdykning
I Go används en s.k. pseudoslumpmässig generator som inte genererar helt slumpmässiga värden, utan baserar sig på en startnummer, eller seed, för att sedan beräkna nästa värde baserat på en matematisk formel. Detta gör att resultatet alltid blir detsamma om man använder samma seed. Genom att använda funktionen `rand.Seed()` med olika parametrar kan man skapa olika sekvenser av slumpmässiga värden.

Det finns också andra faktorer som kan påverka den genererade sekvensen, som till exempel antalet anrop till randgeneratorn och vilka andra funktioner som anropas samtidigt. Det är viktigt att ha detta i åtanke när man använder slumpmässiga nummer i sin kod för att undvika att få oväntade resultat.

## Se även
- [Go dokumentation: paketet math/rand](https://golang.org/pkg/math/rand/)
- [Artikel: Den sanna kostnaden av slumpmässighet i program](https://queue.acm.org/detail.cfm?id=2699322)
- [Video: Tjena_Program Episode #118 - Slumpmässiga nummer](https://www.youtube.com/watch?v=xknZPW1S-ao)