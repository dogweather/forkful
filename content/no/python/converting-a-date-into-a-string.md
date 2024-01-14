---
title:    "Python: Konvertere en dato til en streng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere datoer til strenger er en vanlig oppgave i programmering, spesielt når man jobber med data fra forskjellige systemer og formater. Å kunne konvertere en dato til en streng gjør det enkelt å presentere informasjon til brukere eller lagre det i en database.

## Slik gjør du det

For å konvertere en dato til en streng, kan du bruke `strftime()` -funksjonen i Python. Dette gjør at du kan formatere datoen etter dine spesifikasjoner.

```Python
import datetime

# Opprett et datetime-objekt
d = datetime.datetime(2021, 1, 1)

# Konverter datoen til en streng med ønsket format
streng_dato = d.strftime("%d.%m.%Y")

print(streng_dato)
# Output: 01.01.2021
```

I dette eksempelet har vi brukt `%d` for å få dagen, `%m` for å få måneden og `%Y` for å få årstallet. Du kan også bruke andre formateringsalternativer, som for eksempel `%b` for å få forkortelsen av måneden (f.eks. Jan for januar), eller `%a` for å få forkortelsen av ukedagen (f.eks. Man for mandag). Det finnes mange flere formateringsalternativer som du kan lese mer om i Python-dokumentasjonen.

Du kan også legge til tekst i strengen din, som for eksempel et mellomrom mellom dag, måned og år.

```Python
streng_dato = d.strftime("%d. %b %Y")

print(streng_dato)
# Output: 01. Jan 2021
```

## Dypdykk

Når du konverterer en dato til en streng, er det viktig å være oppmerksom på hvilket format du velger å bruke. Dette kan påvirke hvordan datoen vises for brukere i forskjellige land og regioner. For eksempel, mens noen land bruker måned-dag-år-formatet (f.eks. 01/01/2021), foretrekker andre land dag-måned-år-formatet (f.eks. 01.01.2021).

Det kan også være noen forskjeller i formatering av datoer mellom Python-versjoner, så det er viktig å dobbeltsjekke dokumentasjonen for versjonen du jobber med.

En annen viktig faktor å tenke på er hvilket språk som brukes i strengen. Hvis du ønsker å vise månedsnavn på språket som brukes i brukerens land, kan du bruke `locale` - modulen i Python. Dette sikrer at datoen blir formatert på riktig måte for den gitte regionen.

## Se også

- [Datotime-modulen i Python-dokumentasjonen](https://docs.python.org/3/library/datetime.html)
- [Bruk av datotime-modulen - Real Python tutorial](https://realpython.com/python-datetime/)
- [Formatering av datoer og tider i Python - Python Course](https://www.python-course.eu/python3_formatted_output.php)