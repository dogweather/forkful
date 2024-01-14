---
title:                "Fish Shell: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna få den nuvarande datumen kan vara användbart för olika programmeringsuppgifter. Det kan hjälpa dig att spåra när en viss händelse eller uppgift utfördes eller för att skapa automatiska meddelanden eller påminnelser baserat på datumet. I denna artikel kommer vi att utforska hur man kan använda Fish Shell för att få den aktuella datumet på ett enkelt sätt.

## Så här gör du
För att få den nuvarande datumen i Fish Shell, använd följande kommando i terminalen:

```Fish Shell
date +%Y-%m-%d
```

Detta kommer att ge dig datumet i formatet "år-månad-dag", till exempel "2020-08-25". Om du vill ha datumet i ett annat format, som "dag/månad/år", kan du använda följande kommando:

```Fish Shell
date +%d/%m/%Y
```

Det finns många olika formatteringsalternativ som du kan använda i kombination med "date" kommandot för att få den aktuella datumet på ditt önskade format.

## Deep Dive
För att förstå hur "date" kommandot fungerar bakom kulisserna, låt oss ta en djupare titt på dess syntax. Det följer följande struktur:

```Fish Shell
date [+format] [-u] [MMDDhhmm[[CC]YY][.ss]]
```

Här är några viktiga delar att notera:

- "+" betyder att du lägger till ett formateringsalternativ.
- "-" betyder att du tar bort ett formateringsalternativ.
- "[format]" representerar de olika formateringsalternativen som du kan använda.
- "[MMDDhhmm[[CC]YY][.ss]]" representerar ett specifikt datum eller tid som du vill formatera (valfritt).

Du kan lära dig mer om olika formatalternativ genom att läsa "date" man-sida.

## Se även
- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [date man-sida](https://fishshell.com/docs/current/builtin.html#date)