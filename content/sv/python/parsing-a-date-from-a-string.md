---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att tolka ett datum från en sträng handlar om att omvandla text till ett datumobjekt som din kod kan arbeta med. Programmerare gör detta för att effektivt manipulera, jämföra och beräkna med datumdata som kommit i strängformat, vanligtvis från användarinput eller externela data källor.

## Hur Man Gör:
Python's inbyggda bibliotek `datetime` har en funktion som heter `strptime` som tillåter oss att tolka ett datum från en sträng. Här är ett exempel:

```Python
from datetime import datetime

date_string = "2022-01-01"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)
```
När du kör detta kommer utskriften vara:

```Python
2022-01-01 00:00:00
```
## Fördjupning
Historiskt sett, behovet av att tolka datum från strängar började när data började lagras och överföras i textformat. Detta härrör från tidiga databaser till moderna API:s.

Några alternativ till Pythons `strptime` inkluderar andra bibliotek, som `dateutil` och pandas. `dateutil` kan tolka de flesta strängformat automatiskt, medan pandas passar bra när du arbetar med stora dataset.

Intern, `strptime` skapar en `datetime` instans från de värden den extraherar från strängen. Det standardiserar alltså datum och tid som kan presenteras i myriader av format till en standard form.

## Se Också
1. Python's officiella dokumentation på `datetime`: https://docs.python.org/3/library/datetime.html
2. Detaljerad guide på datum och tid i Python: https://realpython.com/python-datetime/
3. Dokumentation för `dateutil`: https://dateutil.readthedocs.io/en/stable/
4. pandas biblioteket för data manipulation: https://pandas.pydata.org/