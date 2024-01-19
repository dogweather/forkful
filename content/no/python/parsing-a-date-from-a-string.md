---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å analysere en dato fra en streng betyr å konvertere en tekstverdi som representerer en dato til en faktisk datoverdi. Dette er vanligvis gjort for å manipulere datumene for sortering, filtrering eller utføring av andre operasjoner i programmer.

## Hvordan: 
Her er et eksempel på hvordan du gjør det ved hjelp av Python's 'strptime' metode.

```Python
# Importer datetime-modulen
from datetime import datetime

# Definer datostrengen
date_string = "30 April, 2021"

# Analyser datostrengen
date_object = datetime.strptime(date_string, "%d %B, %Y")

print(date_object)
```

Når du kjører denne koden, vil resultatet være:

```Python
2021-04-30 00:00:00
```

## Dyp Dykk
Historisk sett har språk som C og Java også hatt innebygde funksjoner for parsing av dato fra strenger, men Python har blitt mer populært på grunn av sin enkelhet og lesbarhet.

Alternativt kan du bruke 'dateutil' -biblioteket, som gir mer fleksible parsingalternativer. Du kan for eksempel bruke 'dateutil.parser' for å analysere nesten hvilken som helst type datostreng.

Bak scenene bruker 'strptime'-funksjonen en oppslagstabell for å mappe hver del av datostrengen til den faktiske datoenheten.

```Python
# Importer dateutil.parser
from dateutil.parser import parse

# Analyser datostrengen
date_object = parse("30 April, 2021")

print(date_object)
```

Hvis du kjører denne koden, blir outputen:

```Python
2021-04-30 00:00:00
```

## Se Også:
For mer detaljert informasjon, se Python dokumentasjonen på dato og tid: (https://docs.python.org/3/library/datetime.html)

For mer informasjon om 'dateutil.parser', se: (https://dateutil.readthedocs.io/en/stable/parser.html)