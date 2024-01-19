---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Python: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Kalkulering av dato i fremtiden eller fortiden med Python

## Hva & Hvorfor?
Kalkulering av dato i fremtiden eller fortiden implicerer å finne en ny dato basert på en gitt dato og et antall dager. Dette er viktig for programmerere for å løse oppgaver som for eksempel å planlegge hendelser, spore tidsfrister, eller beregne levetiden for en cookie i nettutvikling.

## Slik gjør du:
Python's innebygde `datetime` bibliotek gjør det enkelt å beregne fremtidige og tidligere datoer.

Her er et eksempel:
```python
from datetime import datetime, timedelta

# Gitt dato
dato = datetime.now()

# Beregn en dato 7 dager frem i tid
future_date = dato + timedelta(days=7)
print(future_date)

# Beregn en dato 7 dager tilbake i tid
past_date = dato - timedelta(days=7)
print(past_date)
```
Produksjon av dette koden gir to datoer: dagens dato pluss 7 dager og dagens dato minus 7 dager.

## Deep Dive
Før innføringen av `datetime` biblioteket i Python, måtte utviklere manuelt håndtere mange av de komplekse kalkuleringene rundt tid og dato. Dette innebar å ta hensyn til skuddår, tidssoner og mange andre faktorer.

Alternativt kan du også bruke `dateutil` biblioteket som tilbyr mer omfattende funksjoner for å beregne datoer.

Implementeringsdetaljer: `datetime` biblioteket tilbyr `timedelta` klassen for å representere varigheter. `timedelta` kan håndtere skuddår, varierende lengde på måneder og så videre, noe som gjør det en verdifull ressurs når du jobber med dato og tid.

## Se Også
1. Python's offisielle dokumentasjon på `datetime`: https://docs.python.org/3/library/datetime.html
2. `dateutil` biblioteket: https://dateutil.readthedocs.io/en/stable/
3. En veiledning om hvordan du arbeider med dato og tid i Python: https://realpython.com/python-datetime/