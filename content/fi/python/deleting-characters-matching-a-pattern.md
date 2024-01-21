---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:43:08.648776-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Kun puhumme merkkien poistamisesta kuvion mukaan, tarkoitamme tiettyjä merkkejä sisältävien osien paikantamista ja poistamista merkkijonosta. Tämä toiminto on hyödyllinen, koska se auttaa siistimään ja muokkaamaan tekstidataa - esimerkiksi käyttäjäsyötteiden puhdistamisessa tai ennalta määrättyjen mallien, kuten puhelinnumeroiden, poistamisessa.

## How to (Kuinka tehdä):
```Python
import re

# Esimerkki: Poistetaan kaikki numeroita vastaavat merkit merkkijonosta.
teksti = "Hei! Olen 30-vuotias ja asun postinumeroalueella 00100."
puhdas_teksti = re.sub(r'\d', '', teksti)
print(puhdas_teksti)
```
Tulostus:
```
Hei! Olen -vuotias ja asun postinumeroalueella .
```

```Python
# Esimerkki: Poistetaan välimerkit käyttämällä.
teksti = "Onko sinulla kysymyksiä? Vastaa, kiitos!"
puhdas_teksti = re.sub(r'[?!,.]', '', teksti)
print(puhdas_teksti)
```
Tulostus:
```
Onko sinulla kysymyksiä Vastaa kiitos
```

## Deep Dive (Sukellus syvemmälle):
Merkkien poistaminen kuvion mukaan on ollut osa ohjelmointia jo vuosikymmeniä. Se liittyy vahvasti säännöllisiin lausekkeisiin, jotka tulivat käyttöön 1950-luvulla Stephen Kleenen töiden kautta. Pythonissa `re`-kirjasto tarjoaa rikkaat mahdollisuudet säännöllisten lausekkeiden käsittelyyn.

Vaihtoehtoisia tapoja poistaa merkkejä ovat muun muassa Pythonin perusmetodit, kuten `replace()` tai `translate()`, mutta ne eivät ole yhtä monipuolisia kuin säännölliset lausekkeet.

Yksityiskohtia toteutuksesta: `re.sub()`-funktio ottaa kolme pääargumenttia, jotka ovat kuvio, korvaava merkkijono ja kohde merkkijono. `re.sub(r'\d', '', teksti)` esimerkiksi etsii kaikki numerot (kuvio `r'\d'`) ja korvaa ne tyhjällä merkkijonolla ('') kohde merkkijonossa (`teksti`).

## See Also (Katso myös):
- Pythonin säännölliset lausekkeet: https://docs.python.org/3/library/re.html
- Säännöllisten lausekkeiden opas: https://www.regular-expressions.info/
- Stephen Kleenen tutkimukset: https://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html