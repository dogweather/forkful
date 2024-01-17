---
title:                "Å finne lengden av en streng"
html_title:           "Python: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?
Lengden til en streng er antall tegn i en tekst eller en tekststreng. Dette er nyttig for programmerere fordi det lar dem vite hvor lang en streng er, og hjelper dem med å håndtere og manipulere tekster på en effektiv måte.

# Slik Gjør du det:
For å finne lengden til en streng i Python, bruk funksjonen `len()` og plasser strengen du vil finne lengden til som et argument inne i parentesen. Eksempel,

```Python
tekst = "Hei! Dette er en streng."
print(len(tekst))

Output:
25
```
Her blir lengden til strengen "Hei! Dette er en streng." printet ut som 25. Dette inkluderer også mellomrom og spesialtegn.

# Dybdedykk:
I eldre versjoner av Python ble funksjonen `len()` kalt `count()`, men dette ble endret for å følge en mer konsistent konvensjon innenfor programmering. Alternativt, for å finne lengden til en streng manuelt, kan man bruke en `for`-løkke og øke en teller hver gang en bokstav eller tegn blir funnet. Implementasjonen av `len()`-funksjonen i Python er veldig effektiv og tar bare ett argument for en streng og returnerer en heltallig verdi.

# Se også:
Les mer om `len()`-funksjonen og hvordan den brukes i Python-dokumentasjonen: https://docs.python.org/3/library/functions.html#len