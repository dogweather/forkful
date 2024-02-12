---
title:                "Een datum uit een string parsen"
aliases:
- /nl/python/parsing-a-date-from-a-string/
date:                  2024-01-28T22:04:03.966091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum parsen vanuit een string betekent het omzetten van tekst naar een datumobject. Dit doen we omdat het gemakkelijker is om met data te werken, verschillen te berekenen of ze te formatteren wanneer ze niet vastzitten als simpele tekst.

## Hoe:
De `datetime` module van Python is je beste vriend voor het parsen van data. Hier is een snelle gids:

```python
from datetime import datetime

date_string = "2023-04-01"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)  # Output: 2023-04-01 00:00:00

# Wil je een ander formaat zien? Laten we "dag-maand-jaar" proberen.
another_date_string = "01-April-2023"
another_date_object = datetime.strptime(another_date_string, "%d-%B-%Y")

print(another_date_object)  # Output: 2023-04-01 00:00:00
```

## Diepere Duik
Parsen is essentieel sinds databases en gebruikersinterfaces samen begonnen te werken. Historisch gezien werden gegevens vaak opgeslagen als strings, zelfs data. Nu hebben we echter de `datetime` module die geïntroduceerd werd in Python 2.3 (en sindsdien aanzienlijk is verbeterd).

Je bent niet beperkt tot `datetime`. Je kunt ook gebruikmaken van externe bibliotheken zoals `dateutil`, die flexibeler is met formaten, of `pandas` voor zwaar werk in data-analyse.

Wat implementatie betreft, staat `strptime` voor "string parse time" en gebruikt het format codes om patronen te herkennen. Dit betekent dat je Python het formaat van de datumstring moet vertellen, zoals `%Y` voor een viercijferig jaar of `%d` voor dag.

## Zie Ook
- De documentatie van datetime: https://docs.python.org/3/library/datetime.html
- Dateutil's parser: https://dateutil.readthedocs.io/en/stable/parser.html
- Pandas' to_datetime functie: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html
