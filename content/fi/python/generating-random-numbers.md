---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:59.230752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arvotaan numeroita, koska tarvitaan sattumanvaraisuutta: peleistä, simulaatioista, turvallisuuteen. Python tekee sen helpoksi.

## How to: (Kuinka tehdä:)
```Python
import random

# Satunnainen kokonaisluku väliltä 1-10
numero = random.randint(1, 10)
print(numero)  # Esim. tulostus: 7

# Satunnaisluku liukulukuna väliltä 0-1
liukuluku = random.random()
print(liukuluku)  # Esim. tulostus: 0.4354679041

# Sekoita lista sattumanvaraisesti
lista = [1, 2, 3, 4, 5]
random.shuffle(lista)
print(lista)  # Esim. tulostus: [3, 5, 1, 4, 2]
```

## Deep Dive (Sukellus syvemmälle):
Random-lukujen generointi on vanha konsepti. Alkuperäiset tietokoneet käyttivät erilaisia fyysisiä ilmiöitä sattumanvaraisuuden lähteenä. Nykyään käytetään pseudosatunnaislukugeneraattoreita (PRNG), kuten Mersenne Twister Pythonissa.

Vaihtoehtoja: `numpy`-kirjastossa on omat funktionsa suurien tietoasettien satunnaistamiseen nopeasti, `secrets`-moduuli tarjoaa turvallisemmat numerot, kun tarvitaan kryptografista turvallisuustasoa.

Toteutustiedot: Pythonin `random`-moduuli käyttää PRNG:tä, mikä tarkoittaa, että ne ovat ennustettavia, jos tiedetään alkuarvo (seed). Tämä on hyvä testausta varten, mutta ei turvallisuuden kannalta.

## See Also (Katso myös):
- Pythonin dokumentaatio: https://docs.python.org/3/library/random.html
- numpy random sampling: https://numpy.org/doc/stable/reference/random/index.html
- secrets-moduulin dokumentaatio: https://docs.python.org/3/library/secrets.html
