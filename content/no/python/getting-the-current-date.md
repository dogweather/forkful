---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:16:13.179681-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente gjeldende dato i Python betyr å få tak i dagens dato fra systemet. Programmerere gjør dette for å loggføre hendelser, håndtere dato-sensitive oppgaver, eller bare for å vise tidspunktet for en bruker.

## Hvordan:
```Python
from datetime import date

# Henter dagens dato
dagens_dato = date.today()

# Viser dagens dato
print(f"Dagens dato er: {dagens_dato}")
```
**Sample Output:**
```
Dagens dato er: 2023-04-15
```

## Dypdykk
Historisk sett har henting av dato og tid alltid vært en essensiell del av programmering. Før `datetime` biblioteket i Python, måtte man bruke lavnivåsystemkall for å få tak i tidsdata.

Alternative måter å hente gjeldende dato inkluderer bruk av `time` modulen eller eksterne biblioteker som `arrow` eller `pendulum`, som tilbyr rikere funksjonalitet for håndtering av tid og dato.

Når `date.today()` kalles, interagerer Python med datamaskinens operativsystem for å hente systemklokken sin nåværende dato. Dette retureres som et `date` objekt, som enkelt kan manipuleres og formaters i Python.

## Se Også
- Offisiell dokumentasjon for `datetime` modulen: https://docs.python.org/3/library/datetime.html
- `arrow` biblioteket for enklere dato og tid i Python: https://arrow.readthedocs.io
- `pendulum` biblioteket for avansert dato og tid håndtering: https://pendulum.eustace.io/