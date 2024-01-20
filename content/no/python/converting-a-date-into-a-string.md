---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng innebærer å transformere det numeriske formatet av en dato til en tekstlig representasjon. Dette gjøres mye i programmering for å gjøre informasjonen mer leselig for mennesker, eller for å tillate enkel manipulering og formatendring.

## Hvordan gjør man det:

Her er et grunnleggende eksempel på hvordan man kan konvertere en dato til en streng i Python. Vi bruker datetime-modulen fra Python standardbibliotek.

```Python 
from datetime import datetime

# Skap en dato
dato = datetime(2022, 3, 24)

# Konverter dato til streng
dato_streng = dato.strftime("%d-%m-%Y")

print(dato_streng)
```

Når du kjører dette programmet, vil det skrive ut følgende:
```Python 
'24-03-2022'
```

## Dypdykk

Historisk sett har konvertering av datoer til strenger vært viktig for å omforme data til en form som er lettere å forstå for mennesker. Dette er spesielt viktig i loggføringer og brukergrensesnitt. 

Som et alternativ kan du bruke andre biblioteker som pendulum eller arrow for mer avansert datohåndtering. Disse bibliotekene tillater lett manipulering av dato og tid utover det som Python standardbibliotek tilbyr.

Datoer i Python blir internt representert som antall sekunder siden 1. januar 1970, et tidspunkt kjent som "UNIX Epoch". Strftime-funksjonen brukes til å konvertere dette til en mer lesbar streng ved hjelp av angitte formateringsregler.

## Se også

1. [Python Dokumentasjon DateTime](https://docs.python.org/3/library/datetime.html)

2. [Python Dokumentasjon Time](https://docs.python.org/3/library/time.html)

3. [Pendulum](https://pendulum.eustace.io/docs/)

4. [Arrow](https://arrow.readthedocs.io/en/latest/)