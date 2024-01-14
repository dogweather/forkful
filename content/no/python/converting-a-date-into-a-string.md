---
title:    "Python: Konvertering av en dato til en streng"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere datoer til strenger er en viktig del av enhver programmerers verktøykasse. Det lar deg vise datoer på en lesbar måte i ulike formater, og er spesielt nyttig for å lage brukervennlige grensesnitt og lage rapporter.

## Hvordan

Her er et eksempel på hvordan du kan konvertere en dato til streng i Python:

```Python
from datetime import datetime

today = datetime.now()
date_string = today.strftime("%d.%m.%Y")

print(date_string)
```
Dette vil skrive ut dagens dato i formatet "dd.mm.åååå". Du kan bytte ut "%d.%m.%Y" med ulike formater for å få datoer i forskjellige formater. For eksempel "%A, %d %B %Y" vil gi deg en dato som dette: "torsdag, 30 september 2021".

Du kan også bruke metoder som .format og f-strings for å konvertere datoer til strenger:

```Python
date_string = "{}/{}/{}".format(today.day, today.month, today.year)
f_date_string = f"{today.day}/{today.month}/{today.year}"
```

## Dypdykk

Når du konverterer en dato til en streng, er det viktig å være oppmerksom på at datoen blir formatert etter datamaskinens lokale innstillinger. Dette betyr at hvis du deler koden din med noen fra et annet land, kan datoen vises i et annet format enn forventet.

En annen ting å huske på er at konvertering av datoer til strenger kan være avhengig av språket som brukes i programmet ditt. Dette gjelder spesielt for månedsnavn og ukedagsnavn. Hvis målet ditt er å lage flerspråklige programmer, må du være forsiktig med hvordan du formatterer datoer.

## Se også

- [Offisiell dokumentasjon for datofunksjoner i Python](https://docs.python.org/3/library/datetime.html)
- [En guide til å formatere datoer i Python](https://realpython.com/python-datetime/)
- [Eksempler på formater for dato- og tidsstrukturer i Python](https://strftime.org/)