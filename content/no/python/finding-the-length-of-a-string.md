---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng innebærer å telle antall karakterer i den. Dette er nyttig for programmerere for å håndtere tekstanalyse, validering av datainngang, løkker og mer.

## Hvordan:
Å finne lengden på en streng i Python er ganske enkelt, takket være innebygd funksjon `len()`:

```Python
tekst = "Hei, Norge!"
print(len(tekst))
```
Kjører denne koden vil gi oss utskriften:
```
12
```
## Dyp Dykk
Python's `len()` funksjon kommer fra C under-python's rot som en innebygd funksjon. Alternativer til `len()` inkluderer løkker, men `len()` er vanligvis mer effektiv.

Implementering detaljer er verdt å merke: `len()` bruker ikke faktisk teller tegn i strengen. Den returnerer verdien av et teller som Python har opprettholde i bakgrunnen.

## Se Også
For å lære mer, sjekk ut disse lenkene:
- Python's Offisielle Dokumentasjon på Innebygde Funksjoner: https://docs.python.org/3/library/functions.html#len
- Effektiv Python: 59 Måter å Skrive Bedre Python på: https://effectivepython.com/
- Python Crash Course: En Hands-On, Project-Based Introduction to Programming: https://nostarch.com/pythoncrashcourse2e