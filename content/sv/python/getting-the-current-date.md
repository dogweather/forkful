---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att få det aktuella datumet innebär att programmålande bestämma dagens datum med hjälp av datorns systemklocka. Detta används vid loggning, tidsstämpling, och att skapa datumbaserade funktioner.

## Hur man gör:

För att få det nuvarande datumet i Python kan du använda `datetime` modulen som följer:

```Python
from datetime import date

dagens_datum = date.today()

print(dagens_datum)
```
Sample output:

```Python
2022-04-01
```
Denna kod hämtar dagens datum (år, månad, dag) från din dator och visar det.

## Djupdykning:

Att få det aktuella datumet är inget nytt. Det har använts sedan tiderna för tidiga datorer för att hålla reda på tid och datum. I Python, är det 'datetime' biblioteket det vanligaste sättet att hantera datum och tid, men det finns också andra metoder såsom 'time' modulen.

```Python
import time

tidsstämpel = time.time()

print(tidsstämpel)
```
Sample output:

```Python
1648888285.025971
```
Detta ger en tidsstämpel, i sekunder sedan epoken, vilket är den första januari 1970.

Värdena som returneras av dessa funktioner baseras på systemklockan och beror därför på din dators inställningar och tidzon.

## Se också:

För mer information om att hantera datum och tid i Python, se följande länkar:

- [Officiella Python Dokumentation för 'datetime'](https://docs.python.org/3/library/datetime.html)
- [Python 'time' Modulen](https://docs.python.org/3/library/time.html)