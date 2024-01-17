---
title:                "Konvertere en dato til en streng."
html_title:           "Python: Konvertere en dato til en streng."
simple_title:         "Konvertere en dato til en streng."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng er en viktig del av programmering, spesielt når man håndterer datoer og tidspunkter i koden. Dette gjør det enklere å lagre, sammenligne og presentere datoer i et forståelig format.

## Slik gjør du:
```Python
# Importer datetime modulen
import datetime

# Konvertere en dato til en streng
dato = datetime.date(2021, 9, 18)
dato_til_streng = dato.strftime("%d/%m/%y")

# Skriver ut resultatet
print(dato_til_streng)

# Output: "18/09/2021"
```

## En nærmere titt:
Konvertering av dato til streng er en vanlig praksis i programmering, spesielt når man jobber med tid og dato-funksjoner. Før datetime-modulen ble introdusert i Python 2.3, måtte utviklere selv skrive koden for å håndtere datoer og tidspunkter. Dette var en tidkrevende og feilutsatt prosess. Alternativet til å bruke strftime-metoden er å bruke format-funksjonen, som ble introdusert i Python 3.6.

## Se også:
- [Python datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [Python 3.6 innebygde formateringsalternativer](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)