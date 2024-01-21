---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:50:03.804556-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall i Python brukes til å lage data som ikke følger et bestemt mønster. Programmører gjør dette for spill, simuleringer, sikkerhet, og steder der uforutsigbare resultater er nødvendige.

## How to:
```Python
import random

# Tilfeldig heltall fra 1 til 10
tall = random.randint(1, 10)
print(tall)
# Output: 7 (kan variere)

# Tilfeldig flyttall mellom 0 og 1
flyttall = random.random()
print(flyttall)
# Output: 0.435467129 (kan variere)

# Tilfeldig element fra en liste
frukter = ["Eple", "Banan", "Cherry"]
valgt_frukt = random.choice(frukter)
print(valgt_frukt)
# Output: Banan (kan variere)
```

## Deep Dive
I historien ble tilfeldige tall først generert manuelt gjennom metoder som å kaste terninger eller blande kort. I databehandling startet vi med pseudo-tilfeldige tallgeneratorer (PRNGs) som tar en 'frøverdi' og produserer en sekvens av tall basert på den. De er "pseudo" fordi de følger en kompleks, men bestemt, algoritme. Python bruker Mersenne Twister som standard PRNG, kjent for sin høye periode og bred aksept i applikasjoner.

Python tilbyr også `secrets` modulen for kryptografisk sterke pseudo-tilfeldige tall som er passende for sikkerhets-sensitive applikasjoner.

Det finnes også andre metoder som bruker fysiske fenomener (f.eks. termisk støy, kosmisk stråling) for å produsere virkelige tilfeldige tall. Disse er ofte tilgjengelig via eksterne tjenester eller spesialiserte maskinvareløsninger.

## See Also
- Python's `random` module documentation: https://docs.python.org/3/library/random.html
- Python's `secrets` module documentation for secure randomness: https://docs.python.org/3/library/secrets.html
- En artikkel om forskjellen på pseudo-tilfeldige og virkelige tilfeldige tall: https://www.random.org/randomness/