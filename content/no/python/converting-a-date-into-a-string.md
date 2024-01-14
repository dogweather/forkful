---
title:                "Python: Konvertere en dato til en streng"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du arbeider med Python programmering, vil du ofte støte på situasjoner der du trenger å konvertere en dato til en streng. Dette er en viktig ferdighet for å kunne håndtere datoer og tider effektivt i koden din. I denne blogginnlegget vil vi utforske hvorfor det er viktig å kunne konvertere datoer til strenger og hvordan du kan gjøre det på en enkel måte.

## Hvordan konvertere en dato til en streng i Python

For å konvertere en dato til streng i Python, kan du bruke `strftime` -funksjonen. Denne funksjonen lar deg formatere datoen på en måte som passer for det du trenger å bruke den til. La oss se på et enkelt eksempel:

```Python
from datetime import datetime
today = datetime.today()

print(today.strftime("%d/%m/%Y"))
```

Dette vil gi følgende output:

`14/05/2021`

Som du kan se, bruker vi `%d`, `%m` og `%Y` for å spesifisere formatet på datoen vi vil ha. Disse symbolene representerer dag, måned og år, og du kan bruke flere av dem i samme streng for å få ønsket format.

## Dykk dypere

Nå som du har lært hvordan du kan konvertere en dato til en streng, la oss gå litt dypere inn i hvorfor dette er nyttig. Først og fremst, når du jobber med brukergrensesnitt, vil du ofte trenge å vise datoer til brukeren. I stedet for å bruke standarddatoformatet, kan du formatere datoen til å passe bedre med det visuelle utseendet til grensesnittet ditt.

Videre kan det å ha full kontroll over formatet på datoen også være nyttig når du lagrer eller behandler data i en database. Ved å konvertere datoen til en streng med ønsket format, kan du enkelt sammenligne og analysere datapunkter basert på datoer.

## Se også

Her er noen nyttige ressurser for å lære mer om konvertering av datoer til strenger i Python:

- [The strftime() Method in Python - Programiz](https://www.programiz.com/python-programming/datetime/strftime)
- [Working with Dates and Time in Python - Real Python](https://realpython.com/python-datetime/)
- [Python Date and Time: How to Print Dates in a Friendly Format - Corey Schafer (YouTube video)](https://www.youtube.com/watch?v=eirjjyP2qcQ)

Vi håper denne guiden var nyttig for deg og hjelper deg med å ta kontroll over datoer i koden din. Lykke til med programmeringen!