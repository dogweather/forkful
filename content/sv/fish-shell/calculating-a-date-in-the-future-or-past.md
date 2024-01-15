---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Fish Shell: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför 

Det kan vara användbart att kunna beräkna en datum i framtiden eller förfluten tid för att hålla koll på viktiga händelser eller deadlines.

## Hur man gör

Det finns flera olika sätt att beräkna datum i framtiden eller förfluten tid i Fish Shell. Här är två exempel:

1. Om du vill beräkna ett datum i framtiden kan du använda `date` kommandot tillsammans med syntaxen `date +%Y-%m-%d -d "DATE + NUMBERTIME". Detta kommer att ge dig datumet i den efterfrågade tiden från det ursprungliga datumet. Till exempel, om du vill ha datumet två veckor från idag skulle det se ut så här: 

```Fish Shell

date +%Y-%m-%d -d "now + 2 weeks"
```

Detta kommer att ge dig datumet i formatet YYYY-MM-DD.

2. Om du istället vill beräkna ett datum i förfluten tid, kan du använda samma `date` syntax men med ett negativt nummer efter `DATE`. Till exempel, om du vill ha datumet två veckor före idag skulle det se ut så här:

```Fish Shell

date +%Y-%m-%d -d "now - 2 weeks"
```

 Det kommer att ge dig datumet två veckor före dagens datum i formatet YYYY-MM-DD.

## Djupdykning

Om du vill lära dig mer om att beräkna datum med `date` kommandot, kan du använda `man` kommandot för att få mer detaljerad information. Skriv helt enkelt `man date` i terminalen för att se manualen för detta kommando och dess olika användningsområden.

## Se även

Här är några länkar som kan vara användbara för att lära dig mer om Fish Shell och dess kommandon:

- [Fish Shell officiell hemsida] (https://fishshell.com/)
- [Fish Shell dokumentation] (https://fishshell.com/docs/current/index.html)
- [Fish Shell Cheatsheet] (https://devhints.io/fish)