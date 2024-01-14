---
title:    "Python: Utskrift av feilrettingsutdata"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor bruke utskrift av feilsøking i Python

Det er viktig å kunne finne og rette feil i koden din når du jobber med Python-programmering. Utskrift av feilsøking er en essensiell teknikk for å identifisere og løse disse feilene. Ved å skrive ut relevant informasjon underveis i koden, kan du få et bedre innblikk i hva som skjer under kjøringen av programmet ditt.

## Hvordan

For å skrive ut feilsøkingsmeldinger i Python, kan du bruke funksjonen `print()`. La oss si at du har skrevet et program som skal regne ut arealet av en sirkel basert på brukerens innputte verdier for radius:

```Python
radius = float(input("Skriv inn radiusen til sirkelen: "))
pi = 3.14159
areal = pi * radius**2

print("Arealet av sirkelen er:", areal)
```

Når du kjører programmet, vil du få følgende utskrift:

```
Skriv inn radiusen til sirkelen: 5
Arealet av sirkelen er: 78.53975
```

Vi kan enkelt se at det er en feil i kalkuleringen av arealet, siden ettersom radiusen er satt til 5, bør det riktige arealet være 78.53982 (i stedet for 78.53975). Ved å printe ut mellomverdier i koden, kan du finne ut hvor feilen ligger og fikse den.

## Dypdykk

En av fordelene med å bruke utskrift av feilsøking i Python er at du kan skrive ut ulike typer informasjon basert på dine behov. For eksempel kan du printe ut verdien av en variabel, en spesifikk beskjed eller til og med en feilmelding dersom en betingelse ikke blir oppfylt. Dette gjør det enklere å spore og rette eventuelle feil i koden din.

Det er også mulig å kontrollere hvor mye informasjon som skal printes ut ved å bruke betingelser eller ved å sette inn utskriftsmeldinger i enkle eller doble anførselstegn.

## Se også

- [Dokumentasjon for `print()`funksjonen i Python](https://docs.python.org/3/library/functions.html#print)
- [Artikkel om feilsøking i Python](https://realpython.com/python-debugging-pdb/)