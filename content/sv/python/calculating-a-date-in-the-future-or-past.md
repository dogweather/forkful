---
title:                "Python: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt räkna ut ett datum i framtiden eller förflutet? Kanske ska du planera en resa eller behöver veta vilken dag du fyller år nästa år. Oavsett anledningen kan det vara användbart att kunna göra detta med hjälp av Python-programmering.

## Så här gör du

Att beräkna ett datum i framtiden eller förflutet med Python är ganska enkelt. Först behöver vi importera biblioteket `datetime` som ger oss funktioner för att hantera datum och tid i Python. Sedan kan vi använda funktionen `date` för att skapa ett datumobjekt med ett specifikt år, månad och dag. Här är ett exempel på hur du kan räkna ut ett datum 10 dagar framåt från idag:

```Python
import datetime

idag = datetime.date.today()
tio_dagar_fram = idag + datetime.timedelta(days=10)

print(tio_dagar_fram)
```

Detta kommer att ge oss ett datumobjekt för 10 dagar framåt från dagens datum. Du kan också välja att ange ett specifikt datum istället för att använda `datetime.date.today()` för att beräkna från ett dynamiskt datum.

## Djupdykning

För att kunna göra mer avancerade beräkningar såsom att räkna ut ett datum baserat på specifika veckodagar eller veckonummer, kan vi använda mer avancerade funktioner inom `datetime`-biblioteket. Till exempel kan vi använda funktionen `datetime.weekday()` för att få reda på vilken veckodag ett visst datum ligger på. Här är ett exempel på hur det kan se ut:

```Python
import datetime

datum = datetime.date(2021, 7, 1)

print("Veckodag för den 1:a juli 2021:", datum.weekday())
```

Detta kommer att ge oss utdatan `3` vilket representerar torsdag (6 = söndag, 0 = måndag).

## Se även

- [Python dokumentation om datetime](https://docs.python.org/sv/3/library/datetime.html)
- [Tutorial om datetider i Python](https://www.programiz.com/python-programming/datetime)
- [Beräkna tid mellan två datum med Python](https://stackabuse.com/how-to-calculate-days-between-two-dates-in-python/)