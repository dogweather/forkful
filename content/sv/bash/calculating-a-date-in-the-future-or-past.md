---
title:                "Beräkna ett datum i framtiden eller förflutet"
html_title:           "Bash: Beräkna ett datum i framtiden eller förflutet"
simple_title:         "Beräkna ett datum i framtiden eller förflutet"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller förflutna innebär att ta ett specifikt datum och tillsätta eller subtrahera ett visst antal dagar, veckor, månader eller år. Programmerare gör det bland annat för att schemalägga händelser, generera påminnelser, och spåra projekt i tid.

## Såhär gör man:

I Bash kan du lätt beräkna ett framtida eller förflutna datum med `date` kommandot och `-d` flaggan. 

```Bash
# För att beräkna ett datum 3 dagar framåt:
date -d "+3 days"

# För att beräkna ett datum 1 vecka tillbaka:
date -d "1 week ago"
```
Utskrifterna från dessa kommandon kommer att vara något som:

```Bash
# Utskriften för det första kommandot
Sun May 5 10:23:42 CEST 2022

# Utskriften för det andra kommandot
Mon Apr 29 10:23:42 CEST 2022
```

## Djupdykning

Beräkning av datum i framtiden eller förflutna har en gammal historia inom programmering, eftersom det är en grundläggande del av att hantera tid i program.

Det finns också alternativ till Bash för att utföra dessa beräkningar. Exempelvis, i Python, kan du använda `datetime` biblioteket, medan i JavaScript kan du använda `Date` objektet.

Beräkning av datum med Bash utförs m.h.a. den inbyggda `date` kommandot, vilket gör det möjligt att förflytta framåt eller bakåt i tid genom att specificera en sträng som argument till `-d` flaggan.

## Se även

1. [Advanced Bash-Scripting Guide: Date and Time](http://tldp.org/LDP/abs/html/timedate.html): En omfattande guide till att arbeta med datum och tid i Bash.
2. [Bash `date` Manual](https://man7.org/linux/man-pages/man1/date.1.html): Officiella dokumentationen för `date` kommandot.
3. [Stack Overflow Thread on Date Calculation](https://stackoverflow.com/questions/3249827/): Ett diskussion om olika sätt att beräkna datum i Bash och andra programmeringsspråk.