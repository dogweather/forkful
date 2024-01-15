---
title:                "Beräkning av ett datum i framtiden eller förflutet"
html_title:           "Bash: Beräkning av ett datum i framtiden eller förflutet"
simple_title:         "Beräkning av ett datum i framtiden eller förflutet"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller i det förflutna kan vara användbart i många olika situationer, till exempel när du behöver planera ett evenemang eller hålla koll på förfallodagar för räkningar.

## Så här gör du
För att beräkna ett datum i framtiden eller förflutna i Bash, kan du använda kommandot `date` tillsammans med flaggan `-d` för att specificera ett datum, och `-v` för att lägga till eller subtrahera tid från det datumet.

### Exempel:
```Bash
# För att få datumet och tiden just nu:
date

# För att få datumet och tiden om en vecka:
date -d "+1 week"

# För att få datumet och tiden för 3 månader sedan:
date -d "-3 months"

# Du kan också kombinera flaggorna för större precision, till exempel:
date -d "+2 days +5 hours -30 minutes"

# Om du vill att resultatet ska visas i ett specifikt format, kan du använda flaggan +"FORMAT", till exempel:
date +"%d/%m/%Y"

# Du kan också kombinera formatflaggan med de tidigare flaggorna, till exempel:
date -d "-1 year +2 months" +"%A, %d %B %Y"
```

### Exempeloutput:
```
sön, 14 jun 2020 14:23:06 +0200
sön, 21 jun 2020 14:23:06 +0200
tor, 14 mars 2020 14:23:09 +0100
torsdag, 21 augusti 2025
12/06/2020
tisdag, 17 augusti 2021
```

## Djupdykning
När du använder `date` kommandot för att beräkna datum i förflutna eller framtiden, kan du också använda dig av en rad andra flaggor för att anpassa ditt resultat ytterligare.

### Några användbara flaggor:
- `-u`: Använd UTC-tid istället för din lokala tidzon.
- `-R`: Visa resultatet i RFC 2822-format.
- `-I`: Visa resultatet i ISO 8601-format.
- `-r`: Visa resultatet från ett filens senaste ändringsdatum.
- `-k`: Använd ditt lokala datumformat istället för det amerikanska formatet.
- `-j`: Visa resultatet som ett heltal av dagar sedan 1 januari år 1.
- `-n`: Visa resultatet som antalet sekunder sedan epoken (1 januari 1970 00:00:00 GMT).
- `-s`: Använd ett specifikt datum som utgångspunkt för beräkningen istället för dagens datum och tid.
- `-D`: Specificera datumet med år, månad och dag var för sig.


## Se även
- [Date command in Bash (Ubuntu documentation)](https://help.ubuntu.com/community/date)
- [Bash (programmeringsspråk) Wikipedia](https://sv.wikipedia.org/wiki/Bash_(programmeringsspråk))