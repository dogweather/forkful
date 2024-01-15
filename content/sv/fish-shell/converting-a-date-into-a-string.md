---
title:                "Omvandling av en datum till en sträng"
html_title:           "Fish Shell: Omvandling av en datum till en sträng"
simple_title:         "Omvandling av en datum till en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är användbart när man vill formatera datumet på ett specifikt sätt för att passa ens behov. Det kan vara för att visa datum och tid på ett mer lättläst sätt, eller för att använda det i olika funktioner och skript.

## Hur Man Gör Det

För att konvertera ett datum till en sträng i Fish Shell använder man kommandot `date` tillsammans med flaggan `+FORMAT`, där FORMAT är det önskade formatet för datumet. Till exempel:

```Fish Shell
date +%Y-%m-%d
```

Detta kommer att producera ett format som år-månad-dag, som till exempel 2020-01-01. Man kan också lägga till tider och tidszoner till formatet, till exempel:

```Fish Shell
date +%Y-%m-%d%H:%M:%S%z
```

Detta kommer att producera formatet för år-månad-dag-tid-tidszon, som till exempel 2020-01-01T14:30:00+0000. Det finns många fler formatmöjligheter som man kan utforska genom att läsa dokumentationen för `date`-kommandot.

## Djupdykning

Kommandot `date` konverterar automatiskt det nuvarande datumet och tiden till en sträng. Men man kan också ange ett specifikt datum och tid med hjälp av flaggan `-d`. Till exempel:

```Fish Shell
date -d "2020-10-31 12:30 PM"
```

Detta kommer att producera formatet för år-månad-dag-tid, som till exempel 2020-10-31 12:30. Man kan också använda `date` för att utföra beräkningar med datum, till exempel att lägga till eller subtrahera dagar och tider.

## Se Även

- Fish Shell dokumentation för `date`: https://fishshell.com/docs/current/cmds/date.html
- Mer information om datumformat: https://en.wikipedia.org/wiki/Date_format_by_country