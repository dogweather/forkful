---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 

Generere tilfeldige tall er prosessen med å få datasystemene til å produsere tall som er ubestemmelige av brukeren. Vi programmerere gjør dette for å introdusere enheter for variabilitet og uforutsigbarhet i designene våre.

## Hvordan: 

Du kan generere tilfeldige tall ved hjelp av Python's innebygde `random`-bibliotek. Her er et eksempel på hvordan du genererer et tilfeldig tall mellom 1 og 10.

```Python
import random
tall = random.randint(1,10)
print(tall)
```
Når du kjører denne koden, vil du se et tall mellom 1 og 10.

## Deep Dive: 

Generering av tilfeldige tall har en lang historie i informatikkens verden. Metoder har variert fra fysiske prosesser som termisk støy til matematiske metoder som Mersenne Twister.

I Python har vi mange alternative måter å generere tilfeldige tall, inkludert `random.uniform` for et tilfeldig flyttall mellom to verdier, eller `random.choice` for å plukke et tilfeldig element fra en liste.

Når det gjelder implementasjonsdetaljer, bruker Python's `random`-bibliotek en variant av Mersenne Twister kjent som MT19937. Dette er en såkalt "pseudorandom number generator", fordi mens tallene det produserer ser tilfeldige ut, vil de være nøyaktig de samme hver gang hvis generatorinitialiseringen (`seed`) er lik.

## Se Også: 

- Python's offisielle dokumentasjon for 'random'-modulen: https://docs.python.org/3/library/random.html
- Wikipedia-artikkel om Mersenne Twister: https://no.wikipedia.org/wiki/Mersenne_twister