---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Hvordan: Python gj\xF8r det enkelt \xE5 konvertere datoer til strenger.\
  \ Bruk metoden [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-\u2026"
lastmod: '2024-04-04T02:02:33.558591-06:00'
model: gpt-4-0125-preview
summary: "Python gj\xF8r det enkelt \xE5 konvertere datoer til strenger."
title: Konvertere en dato til en streng
weight: 28
---

## Hvordan:
Python gjør det enkelt å konvertere datoer til strenger. Bruk metoden [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) tilgjengelig på [dato](https://docs.python.org/3/library/datetime.html#date-objects) objekter. Slik gjør du:

```Python
from datetime import datetime

# Hent gjeldende dato og tid
now = datetime.now()

# Konverter den til en streng i formatet: Måned dag, år
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Utdata: Mars 29, 2023 (eller gjeldende dato)

# Format: ÅÅÅÅ-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Utdata: 2023-03-29 (eller gjeldende dato)
```


### Hvordan jeg gjør det

Dette er hvordan jeg får en dato i [ISO 8601](https://www.w3.org/QA/Tips/iso-date) format med tidssoneinformasjon:

```python
def datestamp() -> str:
    """ 
    Gjeldende dato og tid med tidssone i ISO-format.
    """
    return datetime.now().astimezone().isoformat()
```

#### Eksempelutdata:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Dypdykk
Historisk sett har konvertering av datoer til strenger vært en grunnleggende del av programmering på grunn av behovet for å representere datoer i et menneskelesbart format.

Alternativer til `strftime` inkluderer bruk av `isoformat`-metoden for ISO 8601-format, eller tredjepartsbiblioteker som `arrow` og `dateutil` som tilbyr mer fleksible tolknings- og formateringsalternativer.

Implementeringsmessig står `strftime` for "string format time" og har røtter i C-programmering. Pythons `strftime` tolker formateringskoder som `%Y` for året og `%m` for måneden, noe som tillater nesten uendelig tilpasningsevne.

## Se også
For å dykke dypere inn i Pythons dato- og tidsfunksjoner:
- Pythons offisielle `datetime` dokumentasjon: https://docs.python.org/3/library/datetime.html
- For de som er interessert i en omfattende liste over `strftime` direktiver: https://strftime.org/
- For å utforske tredjeparts dato/tid-biblioteker:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
