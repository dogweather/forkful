---
title:    "Python: Få gjeldende dato"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen kan virke som en enkel oppgave, men det er en viktig del av programmering. Å kunne få og manipulere datoer kan være avgjørende for å lage funksjonelle og nøyaktige programmer. I denne bloggposten vil vi se på hvordan du kan få den nåværende datoen ved hjelp av Python.

## Hvordan

For å få den nåværende datoen i Python bruker vi "datetime" modulen. Først importerer vi modulen ved å skrive følgende kode:

```Python
import datetime
```

Deretter kan vi bruke "datetime.now()" funksjonen for å få den nåværende datoen og klokkeslettet. Dette er et eksempel på hvordan du kan skrive koden:

```Python
dato = datetime.now()
print(dato)
```

Dette vil gi følgende utdata:

2021-05-10 13:35:20.794843

Som du kan se, gir funksjonen oss både dato og klokkeslett. Men hva om vi bare vil ha datoen? Da kan vi bruke "date()" funksjonen, som tar bort klokkeslettet og bare gir oss datoen. Her er et eksempel:

```Python
dato = datetime.now().date()
print(dato)
```

Dette vil gi følgende utdata:

2021-05-10

Vi kan også formatere datoen slik vi ønsker ved å bruke "strftime()" funksjonen. Her er et eksempel på hvordan vi kan få datoen til å se ut som "10.05.2021":

```Python
dato = datetime.now().strftime("%d.%m.%Y")
print(dato)
```

Dette vil gi følgende utdata:

10.05.2021

## Deep Dive

Nå som du vet hvordan du kan få den nåværende datoen ved hjelp av Python, la oss se på noen andre måter å manipulere og lagre datoen på.

Vi kan for eksempel legge til eller trekke fra dager fra den nåværende datoen ved hjelp av "timedelta" objektet. Det kan være nyttig hvis vi for eksempel vil finne ut hva datoen vil være en uke fra nå. Her er et eksempel:

```Python
from datetime import timedelta
dato = datetime.now() + timedelta(days=7)
print(dato)
```

Dette vil gi følgende utdata:

2021-05-17 13:35:20.794843

Vi kan også konvertere en streng til en dato-objekt ved hjelp av "strptime()" funksjonen. Dette kan være nyttig hvis vi for eksempel har en brukerinput og ønsker å konvertere den til en dato. Her er et eksempel:

```Python
dato = datetime.strptime("2021-05-20", "%Y-%m-%d")
print(dato)
```

Dette vil gi følgende utdata:

2021-05-20 00:00:00

Det finnes mange flere måter å arbeide med datoer på i Python, og det er lurt å utforske dem for å bli mer komfortabel med å manipulere og lagre datoer i programmene dine.

## Se også

- "Python dokumentasjon om datetime modulen" (https://docs.python.org/3/library/datetime.html)
- "Tutorialspoint tutorial om arbeid med datoer i Python" (https://www.tutorialspoint.com/python/python_date_time.htm)
- "W3 Schools tutorial om date og datetime objekter i Python" (https://www.w3schools.com/python/python_datetime.asp)