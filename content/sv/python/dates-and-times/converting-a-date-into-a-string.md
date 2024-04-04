---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Hur man g\xF6r: Python g\xF6r det enkelt att konvertera datum till str\xE4\
  ngar. Anv\xE4nd metoden\u2026"
lastmod: '2024-04-04T02:02:45.448241-06:00'
model: gpt-4-0125-preview
summary: "Python g\xF6r det enkelt att konvertera datum till str\xE4ngar."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur man gör:
Python gör det enkelt att konvertera datum till strängar. Använd metoden [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) som finns tillgänglig på [datum](https://docs.python.org/3/library/datetime.html#date-objects) objekt. Så här gör du:

```Python
from datetime import datetime

# Hämta aktuellt datum och tid
now = datetime.now()

# Konvertera det till en sträng i formatet: Månad dag, År
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Utdata: Mars 29, 2023 (eller aktuellt datum)

# Format: ÅÅÅÅ-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Utdata: 2023-03-29 (eller aktuellt datum)
```


### Hur jag gör det

Så här får jag ett datum i [ISO 8601](https://www.w3.org/QA/Tips/iso-date) format med tidszonsinfo:

```python
def datestamp() -> str:
    """ 
    Aktuellt datum och tid med tidszon i ISO-format.
    """
    return datetime.now().astimezone().isoformat()
```

#### Exempelutdata:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Fördjupning
Historiskt har konvertering av datum till sträng varit en grundpelare i programmering på grund av behovet av att representera datum i ett läsbart format för människor. 

Alternativ till `strftime` inkluderar att använda `isoformat` metoden för ISO 8601 format, eller tredjepartsbibliotek som `arrow` och `dateutil` som erbjuder mer flexibla tolknings- och formateringsalternativ.

Implementationen av `strftime` står för "string format time" och har sina rötter i C programmering. Pythons `strftime` tolkar formatkoder som `%Y` för året och `%m` för månaden, vilket möjliggör nästan oändlig anpassningsbarhet.

## Se även
För att fördjupa dig i Pythons datum- och tidsfunktioner:
- Pythons officiella `datetime` dokumentation: https://docs.python.org/3/library/datetime.html
- För de som är intresserade av en omfattande lista av `strftime` direktiv: https://strftime.org/
- För att utforska tredjeparts datum-/tidbibliotek:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
