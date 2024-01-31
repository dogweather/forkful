---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:38:34.877429-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Datumtolkning från strängar är omvandlingen av text till ett datumformat som Python förstår. Programutvecklare gör detta för att enkelt hantera och jämföra datum, utföra tidsberäkningar eller lagra datum i en mer effektiv form.

## How to:
Använd `datetime.strptime` för att tolka datumsträngar och `strftime` för att formatera datum som strängar. Exempel:

```python
from datetime import datetime

# Tolkar en datumsträng till ett datetime-objekt
datum_strang = '2023-04-01'
datum_objekt = datetime.strptime(datum_strang, '%Y-%m-%d')
print(datum_objekt)

# Formaterar ett datetime-objekt till en sträng
formaterat_datum = datum_objekt.strftime('%d %B %Y')
print(formaterat_datum)
```

Utskrift:
```
2023-04-01 00:00:00
01 April 2023
```

## Deep Dive
From att datum kan skrivas på så många sätt världen över, utvecklade man tidigt funktioner för datumtolkning. Innan `datetime.strptime` användes ofta tidsstämplar, skript och externa verktyg för konvertering av datum. Alternativ till `strptime` inkluderar användning av tredjepartsbibliotek som `dateutil.parser` som är mer förlåtande med format:

```python
from dateutil import parser
datum = parser.parse('1st of April, 2023')
print(datum)
```

Implementationen i Python använder bakomliggande C-funktioner, vilket innebär att en del av prestandan och funktionaliteten är beroende av plattformen.

## See Also
- Python's datetime documentation: https://docs.python.org/3/library/datetime.html
- Dateutil's documentation: https://dateutil.readthedocs.io/en/stable/
- Python's time module for working with time: https://docs.python.org/3/library/time.html
