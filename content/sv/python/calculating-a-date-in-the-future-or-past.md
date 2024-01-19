---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Python: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Artikel: Beräkna datum i framtiden eller förflutna med Python

## Vad & Varför?
Beräkning av datum i framtiden eller förflutna innebär att hitta ett specifikt datum baserat på ett annat datum och ett intervall, som kan vara i dagar, veckor, månader, etc. Programmerare gör detta för att spåra händelser, hantera återkommande uppgifter eller bara för att hantera tidsdatauppdrag.

## Hur gör man:
Nedan finns en kodsnutt i Python för att beräkna framtida och förflutna datum.

```python
import datetime

# dagens datum
nu = datetime.datetime.now()

# beräkna ett datum 5 dagar framåt
framtida_datum = nu + datetime.timedelta(days=5)
print("Framtida datum: ", framtida_datum.date())

# förflutet datum 3 dagar tillbaka
forflutet_datum = nu - datetime.timedelta(days=3)
print("Förflutet datum: ", forflutet_datum.date())
```

Exempel output:

```shell
Framtida datum: 2022-11-16
Förflutet datum: 2022-11-08
```

## Djupdykning
Beräkning av datum i framtiden och förflutet är en gemensam uppgift i programmering och har varit det sedan tidiga dagar inom mjukvaruutveckling. Python gör detta enkelt med `datetime` och `timedelta` moduler, men det finns också andra alternativ som `dateutil.relativedelta` som kan hantera mer komplexa beräkningar.

Det finns ett antal sätt att implementera detta i Python. Ovanstående exempel använder `timedelta`, vilken representerar en varaktighet, skillnaden mellan två datum eller tider.

```python
from dateutil.relativedelta import relativedelta

nu = datetime.datetime.now()

# framtida datum 1 månad framåt
framtida_datum = nu + relativedelta(months=1)
print("Framtida datum: ", framtida_datum.date())

# förflutet datum 1 år tillbaka
forflutet_datum = nu - relativedelta(years=1)
print("Förflutet datum: ", forflutet_datum.date())
```

## Se också
1. [Python officiella dokumentation för datetime](https://docs.python.org/3/library/datetime.html)
2. [Python officiella dokumentation för dateutil](https://dateutil.readthedocs.io/en/stable/)
3. [Datetime - Python Modul av Veckan](https://pymotw.com/3/datetime/)