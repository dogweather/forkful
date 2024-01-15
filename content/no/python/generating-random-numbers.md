---
title:                "Tilfeldig tallgenerering"
html_title:           "Python: Tilfeldig tallgenerering"
simple_title:         "Tilfeldig tallgenerering"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan datamaskiner kan generere tilfeldige tall? Å forstå hvordan man kan produsere tilfeldige tall er en viktig del av programmering og kan være nyttig for flere forskjellige applikasjoner, som spill og kryptering.

## Hvordan lage tilfeldige tall i Python 

Det er flere måter å generere tilfeldige tall i Python på, og det er også flere ulike funksjoner som kan brukes til dette formålet. Her er to enkle eksempler på hvordan du kan generere tilfeldige tall i Python:

```Python
# Importer random modulen
import random

# Generer et tilfeldig heltall mellom 0 og 100
random_number = random.randint(0, 100)
print(random_number)
```

```Python
# Importer random modulen
import random

# Generer et tilfeldig desimaltall mellom 0 og 1
random_decimal = random.uniform(0, 1)
print(random_decimal)
```

Output:

```
49
0.7856345192
```

## Dypdykk

I bakgrunnen bruker disse funksjonene et pseudorandom-nummergenerator (PRNG) for å generere tilfeldige tall. Dette betyr at tallene ikke er helt tilfeldige, men er basert på en matematisk algoritme. PRNG-er produserer sekvenser av tall som er deterministiske, noe som betyr at den samme sekvensen vil bli generert hver gang koden kjøres med de samme startverdiene. Dette kan være nyttig for testing og feilsøking, men det er også viktig å være klar over for sikkerhetsformål.

En annen viktig ting å merke seg er at disse funksjonene ikke vil generere ekte tilfeldige tall hvis de ikke blir gitt en startverdi, også kalt en "seed". Dette er fordi de bruker datamaskinens nåværende tid eller noe annet som startverdi dersom ikke noe er spesifisert.

## Se også

- [Dokumentasjon for random modulen i Python (engelsk)](https://docs.python.org/3/library/random.html)
- [Generering av tilfeldige tall i Python (engelsk)](https://realpython.com/python-random/)