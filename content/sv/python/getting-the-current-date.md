---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:02.533870-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hämta det aktuella datumet är grundläggande: det innebär att få systemets nuvarande datum. Programmerare gör detta för loggning, tidsstämplar eller funktioner som är dato-beroende.

## Hur man gör:
```Python
from datetime import date

# Hämta dagens datum
dagens_datum = date.today()
print("Dagens datum:", dagens_datum)
```

Förväntad utmatning:
```
Dagens datum: 2023-04-05  # Detta datum kommer att variera beroende på när koden körs.
```

## Djupdykning:
Datum och tidshantering har länge varit en central del av programmering. I Python sköts detta genom `datetime` modulen som introducerades i version 2.3. Alternativ för att hämta datum inkluderar att använda tredjepartspaket som `arrow` eller `pendulum`, men `datetime` är standard och välintegrerat i ekosystemet. Funktionen `today()` returnerar ett `date` objekt som representerar det nuvarande lokala datumet och är inte tidzon-medvetet. Detta är tillräckligt för de flesta användningsfall men om man behöver hantera specifika tidszoner använder man `pytz` biblioteket tillsammans med `datetime`.

## Se även:
- Python `datetime` modul: https://docs.python.org/3/library/datetime.html
- PyTZ biblioteket: https://pypi.org/project/pytz/
- Arrow: https://arrow.readthedocs.io
- Pendulum: https://pendulum.eustace.io
