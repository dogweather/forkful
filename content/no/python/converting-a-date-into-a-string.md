---
title:                "Python: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være en nyttig ferdighet når du jobber med Python-programmering. Dette kan være spesielt nyttig når du ønsker å vise datoen i et bestemt format eller for å kunne håndtere datoer som en del av en tekststreng.

## Hvordan

For å konvertere en dato til en streng i Python, kan du bruke funksjonen `strftime()` fra `datetime`-modulen. Denne funksjonen lar deg formatere en datoobjekt til en streng etter ønsket format. Her er et eksempel på hvordan du kan gjøre dette:

```python
# Importer datetime-modulen
import datetime

# Opprett et datetime-objekt med ønsket dato
dato = datetime.datetime(2020, 9, 23)

# Konverter dato til en streng med ønsket format
dato_til_streng = dato.strftime("%d.%m.%Y")

# Skriv ut resultatet
print(dato_til_streng)
```

Output: `23.09.2020`

Her bruker vi `%d` for å få dato og `%m` for å få måned i tosifret format, og `%Y` for å få årstallet i fire sifre. Du kan også bruke andre formateringsalternativer som er tilgjengelige i `strftime()`-funksjonen. Dette inkluderer å vise ukedag, klokkeslett og mer.

## Dypdykk

Når du jobber med datoer og strenger, er det viktig å være oppmerksom på formateringsfeil. For eksempel kan du oppleve at koden ikke fungerer som forventet hvis du prøver å formatere en dato som er i en annen tidssone enn standarden for ditt system. Dette kan skje fordi `datetime`-modulen bruker systemets standardinnstillinger for å formatere datoen.

For å unngå dette problemet, kan du bruke `localtime()` eller `utcnow()`-funksjonene fra `datetime`-modulen for å konvertere datoen til riktig tidssone. Du kan også bruke `pytz`-modulen for å håndtere tidssoner.

## Se også

- [Dokumentasjon for `strftime()`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Dato og tid i Python](https://realpython.com/python-datetime/)
- [Håndtering av tidssoner i Python](https://realpython.com/python-time-zone/)