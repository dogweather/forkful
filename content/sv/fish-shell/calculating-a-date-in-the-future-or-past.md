---
title:                "Fish Shell: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna datum i framtiden eller förfluten tid är en användbar förmåga inom programmering. Det kan till exempel vara användbart för att automatisera uppgifter som att schemalägga händelser eller för att hålla koll på förflutna händelser.

## Hur man gör det
```Fish Shell``` är ett kraftfullt program som kan hjälpa dig att enkelt beräkna datum i framtiden eller förfluten tid. För att göra det, följ dessa enkla steg:

1. Använd kommandot `date` för att visa det aktuella datumet och tiden.
2. Om du vill beräkna ett datum i framtiden, lägg till antalet dagar, veckor eller månader som du vill gå framåt efter datumkommandot. Till exempel, `date --date "+1 week"` kommer att visa datumet om en vecka från idag.
3. På samma sätt, om du vill beräkna ett datum i förfluten tid, lägg till antalet dagar, veckor eller månader som du vill gå bakåt efter datumkommandot. Till exempel, `date --date "-3 months"` kommer att visa datumet för tre månader sedan.

## Fördjupning
Det finns många andra användbara sätt att beräkna datum med ```Fish Shell```. Till exempel kan du använda olika format för datumet som ska visas och ange en specifik tidszon för beräkningen. Du kan också använda variabler för att beräkna datum baserat på andra variablers värden.

## Se även
- [Fish Shell's officiella hemsida](https://fishshell.com/)
- [Dokumentation för datumkommandot i Fish Shell](https://fishshell.com/docs/current/commands.html#date)
- [En användbar guide för att beräkna datum med Fish Shell](https://devpro.media/berechne-daten-in-fish-shell/)