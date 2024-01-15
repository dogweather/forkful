---
title:                "Generera slumpmässiga nummer"
html_title:           "Fish Shell: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Randomiseringsfunktioner är ett viktigt verktyg för programmerare eftersom de hjälper till att skapa slumpvisa nummer. Detta kan vara användbart för olika ändamål, som att skapa säkra lösenord, testa algoritmer eller skapa variation i spel eller underhållningsprogram.

## Så här gör du

Randomiseringsfunktioner finns tillgängliga i Fish Shell för att generera slumpvisa tal. Det finns olika metoder som kan användas beroende på dina behov.

#### Slumpmässig sekvens

För att generera en slumpmässig sekvens av tal i ett visst intervall kan du använda kommandot `seq`. Till exempel kan du skapa en slumpmässig sekvens med 10 tal mellan 1 och 100:

```Fish Shell
seq 1 100 | shuf
```

Detta kommer att producera en blandad sekvens av tal som du kan använda för olika ändamål.

#### Slumpmässig text

Om du behöver generera slumpmässig text kan du använda `pwgen` kommandot. Detta kommando kan ta emot olika argument för att specificera längd och innehåll av den slumpmässiga texten. Till exempel kan du skapa ett slumpmässigt lösenord med 16 tecken som innehåller siffror och specialtecken:

```Fish Shell
pwgen -s -y 16 1
```

#### Slumpmässig dag

Om du behöver generera en slumpmässig dag i ett visst tidsintervall kan du använda kommandot `date` tillsammans med `shuf`. Till exempel kan du skapa en slumpmässig dag mellan det nuvarande datumet och två veckor framåt:

```Fish Shell
shuf -i $(date +%s)-$(date -d '+2 weeks' +%s) -n 1 -t
```

Detta kommer att producera en slumpmässig dag i Unix-tidsformat, som du kan konvertera till det önskade datum- och tidsformatet.

## Djupdykning

Fish Shell innehåller flera inbyggda kommandon för att generera slumpmässiga värden, inklusive `seq`, `shuf` och `pwgen`. Du kan också använda modifieringsoperatörer som `shuf -r` för att repetera värden eller `shuf -n` för att generera ett angivet antal värden. För mer information om dessa kommandon och deras användning, kan du kolla dokumentationen för Fish Shell eller söka efter specifika exempel online.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [Randomiseringsfunktioner i Bash](https://www.cyberciti.biz/faq/bash-shell-generate-random-password/)
- [Generera slumpmässiga datum i Fish Shell](https://unix.stackexchange.com/questions/88346/how-to-generate-a-random-date-and-time-for-a-certain-time-interval)