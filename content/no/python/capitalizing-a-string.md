---
title:                "Å sette en streng i store bokstaver"
html_title:           "Python: Å sette en streng i store bokstaver"
simple_title:         "Å sette en streng i store bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Gjøre store bokstaver i en streng, eller "string", er å endre det første bokstavet i en tekststreng til en stor bokstav. Programmerere gjør dette for å forbedre lesbarhet og for gnistrende tekstbehandling.

## Hvordan det gjøres:

Du kan gjøre dette i Python ved hjelp av innebygd `capitalize()` funksjonen. Her er et eksempel på bruken av det:

```python
text = "heisann, verden!"
capitalized_text = text.capitalize()

print(capitalized_text)
```
Når du kjører dette, vil utdata være:

```
Heisann, verden!
```

Som du kan se, har det første bokstavet i strengen blitt endret til en stor bokstav.

## Dypere inn i detaljene:

Historisk sett har programmerere søkt å forbedre lesbarhet og presentasjon av tekst ved bruk av slike metoder som "capitalization". Imidlertid har noen programmeringsspråk forskjellige tilnærminger til dette.

I Python brukes `capitalize()` funksjonen, men du må være oppmerksom på at den kun gjør det første bokstavet til en stor bokstav. Den gjør alle andre bokstaver om til små bokstaver. For eksempel:

```python
text = "hEISeNN, VeRDeN!"
capitalized_text = text.capitalize()

print(capitalized_text)
```
Resultatet er:

```
Heisann, verden!
```

Hvis du trenger å beholde de opprinnelige store bokstavene i teksten, bør du vurdere å bruke `title()` funksjonen.

## Se også:

For mer informasjon og relaterte ressurser, sjekk ut følgende lenker:
1. Python offisielle dokumentasjon på streng metoder: https://docs.python.org/3/library/stdtypes.html#string-methods
2. Detaljert guide om `capitalize()` og `title()` funksjoner: https://www.w3schools.com/python/ref_string_capitalize.asp
3. Tips og triks for å arbeide med strenger i Python: https://realpython.com/courses/working-python-strings/