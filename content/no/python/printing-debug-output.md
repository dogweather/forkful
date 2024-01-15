---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Python: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har støtt på feil eller problemer i koden din, vet du hvor frustrerende det kan være å prøve å finne ut hva som går galt. Her kommer debugging inn i bildet - og en av de nyttigste verktøyene for å løse disse problemene er å bruke debug output. Ved å skrive ut variabler eller meldinger fra koden din kan du se nøyaktig hva som skjer i hvert trinn av kjøringen, og finne ut hvor feilen ligger.

## Hvordan

Det første trinnet for å bruke debug output i Python er å importere `print()` funksjonen:

```Python
from __future__ import print_function
```

Deretter kan du bruke `print()` funksjonen til å skrive ut hva som helst du vil sjekke i koden din. La oss si at du har en variabel som heter `x` og du vil skrive ut verdien av den for å sjekke om det er riktig:

```Python
x = "Hello"
print(x)
```

Dette vil produsere følgende output når du kjører koden:

```Python
Hello
```

Du kan også legge til flere variabler eller strenger for å få mer informasjon:

```Python
x = "Hello"
y = "World"
print(x, y)
```

Dette vil produsere følgende output:

```Python
Hello World
```

Du kan også bruke `print()` for å sjekke om betingelser blir oppfylt i `if` og `else` setninger:

```Python
x = 5
if x > 10:
    print("x er større enn 10")
else:
    print("x er mindre enn 10")
```

Dette vil gi følgende output, som viser at betingelsen ikke blir oppfylt og at koden går til `else` setningen:

```Python
x er mindre enn 10
```

## Deep Dive

En annen nyttig måte å bruke debug output er å bruke `print()` til å sjekke verdien av variabler i løkker. Ved å skrive ut verdien etter hvert steg vil du kunne se hvordan den endrer seg og om den forventede verdien blir nådd.

Du kan også legge til en ekstra parameter i `print()` funksjonen som viser hvilken linje i koden konsollen skriver ut fra. Dette kan være nyttig hvis du har mange print-statements eller arbeider med flere filer, og ønsker å vite nøyaktig hvilken del av koden som produserte visse output.

```Python
print("Dette er linje 10:", x)
```

## Se også

Hvis du ønsker å lære mer om debugging og hvordan du kan bruke det effektivt i Python, kan du sjekke ut følgende ressurser:

- [Offisiell dokumentasjon for debugging i Python](https://docs.python.org/3/library/pdb.html)
- [En guide til å lage effektive print-statements i koden din](https://realpython.com/python-debugging-pdb/)