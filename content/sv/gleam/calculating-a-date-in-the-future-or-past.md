---
title:    "Gleam: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutna är en viktig del av många program som hanterar tidsbaserade uppgifter. Genom att använda Gleam programming language kan du enkelt implementera denna funktion och effektivt hantera datumberäkningar i din kod.

## Hur man gör

För att beräkna ett datum i framtiden eller förflutna behöver du först importera standardbiblioteket "time" genom att lägga till följande kod i din Gleam-fil:

``` Gleam
import time
```

Därefter kan du använda funktionen `add` för att lägga till en viss tidsperiod till ett angivet datum. Till exempel, om du vill beräkna datumet 30 dagar framåt från idag kan du använda följande kod:

```Gleam
let future_date = add(time.now(), {days = 30})
```

På samma sätt, om du vill beräkna datumet 6 månader tillbaka från dagens datum, kan du använda:

```Gleam
let past_date = add(time.now(), {months = -6})
```

Det är också möjligt att beräkna ett datum baserat på ett specifikt datum. Om du till exempel vill beräkna datumet 1 år framåt från 17 mars 2022, kan du använda:

```Gleam
let specific_date = add({year = 2022, month = 3, day = 17}, {years = 1})
```

Det är viktigt att notera att alla tidsperioder måste anges i förhållande till den nuvarande tiden. Till exempel, om du vill beräkna datumet 10 dagar framåt från den 1 januari 2022, måste du också ange året, månaden och dagen i `add` funktionen.

## Djupdykning

I Gleam finns det möjlighet att använda modulen `Time` för att utföra mer avancerade tidsberäkningar. Modulen har flera funktioner som kan vara användbara för att manipulera datum och tider, till exempel `subtract` för att dra bort en viss tidsperiod från ett datum och `compare` för att jämföra två datum.

Det är också möjligt att använda Gleams inbyggda typer `Time.Time` och `Time.Timezone` för att hantera tid och tidszoner på ett mer robust sätt.

## Se även

- [Gleam Dokumentation - Time](https://gleam.run/documentation/modules/time/)
- [Gleam Blogginlägg - Handling Tid i Gleam](https://gleam.run/news/handling-time-in-gleam/)