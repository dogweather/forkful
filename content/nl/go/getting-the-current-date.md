---
title:                "Het huidige datum ophalen"
date:                  2024-01-28T22:00:50.479562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

De huidige datum in Go krijgen houdt in dat je de datum van het huidige moment uit het systeem haalt. Programmeurs houden tijd bij om gebeurtenissen te timestampen, taken te plannen, of simpelweg de huidige datum aan gebruikers te tonen.

## Hoe:

Hier is hoe je de huidige datum in Go krijgt:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentTime := time.Now()
    fmt.Println("Huidige Datum is:", currentTime.Format("2006-01-02"))
}
```

Als je dit uitvoert, zou je een uitvoer krijgen die lijkt op:
```
Huidige Datum is: 2023-04-05
```
Het formaat `"2006-01-02"` kan er vreemd uitzien, maar in Go is deze specifieke volgorde het referentieopmaak voor datumnotatie.

## Diepgaand

Voor Go hadden programmeertalen elk hun eigen eigenzinnige manieren om met datums en tijden om te gaan. Go vereenvoudigt dit, grotendeels. Waarom de getallen `2006-01-02 15:04:05`? Ze zijn een ezelsbruggetje - de 1e tot en met de 7e getallen in oplopende volgorde, die overeenkomen met `jaar-maand-dag uur:minuut:seconde`.

Er zijn alternatieven zoals het gebruik van Unix-tijd (seconden sinds 1 januari 1970) maar dat is minder leesbaar voor mensen. Het `time` pakket in Go biedt hulpmiddelen voor beide formaten en meer, rekening houdend met tijdzones en zomertijd. Intern communiceert `time.Now()` met de klok van je systeem om de huidige datum en tijd te halen.

## Zie Ook

Voor meer details over het Go `time` pakket, bekijk de officiële documentatie:
- Go `time` pakket: https://pkg.go.dev/time

Om meer te leren over het formatteren en parsen van datums in Go:
- Go by Example - Tijd Formattering / Parsen: https://gobyexample.com/time-formatting-parsing

Begrip van tijdzones in Go programmering:
- Werken met Tijdzones in Go: https://www.alexedwards.net/blog/working-with-time-zones
