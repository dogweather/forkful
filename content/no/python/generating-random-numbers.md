---
title:                "Python: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver, enten det er for spill, sikkerhet eller statistikkformål.

## Slik gjør du det
```Python
# Importer random-modulen
import random

# Generer et tilfeldig heltall mellom 0 og 100
tilfeldig_tall = random.randint(0, 100)
print("Tilfeldig tall:", tilfeldig_tall)

# Generer et tilfeldig desimaltall mellom 0 og 1
tilfeldig_desimal = random.random()
print("Tilfeldig desimaltall:", tilfeldig_desimal)

# Generer et tilfeldig tall fra en liste av mulige verdier
mulige_verdier = [1, 2, 3, 5, 8, 13, 21]
tilfeldig_verdi = random.choice(mulige_verdier)
print("Tilfeldig verdi fra liste:", tilfeldig_verdi)
```

Output:
```
Tilfeldig tall: 85
Tilfeldig desimaltall: 0.587092479252548
Tilfeldig verdi fra liste: 5
```

## Dypdykk
Random-modulen i Python bruker en algoritme kalt Mersenne Twister for å generere tilfeldige tall. Denne algoritmen er basert på matematiske beregninger og en startverdi som kalles seed. Hvis du ikke angir en seed, vil Python bruke datotid som seed, noe som gjør genererte tall helt unike hver gang koden kjøres.

## Se også
- [Python dokumentasjon om random-modulen](https://docs.python.org/3/library/random.html)
- [Stack Overflow: How to generate a random number in Python](https://stackoverflow.com/questions/3996904/generate-random-integers-between-0-and-9/3996945#3994)