---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:14:19.459122-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta dagens datum innebär att få fram det aktuella datumet som en sträng eller ett tal. Programmerare gör detta för att logga händelser, sortera data eller som en del av användargränssnitt.

## Hur man gör:
```Fish Shell
# Hämta nuvarande datum och tid
set -l nu (date)
echo $nu

# Resultat: tis 25 apr 2023 15:42:36 CEST
```

```Fish Shell
# Hämta enbart datumet i formatet ÅÅÅÅ-MM-DD
set -l datum (date "+%Y-%m-%d")
echo $datum

# Resultat: 2023-04-25
```

## Djupdykning:
Historiskt sett har kommandon som `date` använts i shell-skript sedan de tidiga dagarna av operativsystem med tidsdelning. I Fish Shell använder du liknande kommandon som i andra skal, men syntaxen kan vara lite annorlunda, då Fish inte följer POSIX-standarder till fullo.

Alternativ till `date` kan vara inbyggda Fish-funktioner eller externa verktyg som `ntpdate` för att synkronisera tiden via nätet. Detaljmässigt använder `date` systemklockan för att generera datum och tidsinformation. Du kan ändra formatet med argumentet `+"%Y-%m-%d"` där varje specifierare ger en del av datumet som år, månad eller dag.

## Se även:
- Fish dokumentation för datum och tidsvariabler: https://fishshell.com/docs/current/index.html#variables-date-and-time
- GNU Coreutils `date` dokumentation för olika formatoptioner: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
