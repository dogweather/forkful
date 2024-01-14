---
title:                "Python: Sattumanvaraisten lukujen generointi"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmat ja pelit käyttävät satunnaisia numeroita erilaisia toimintoja varten, kuten arvon arvaamiseen tai uniikkien elementtien luomiseen. Satunnaiset numerot ovat tärkeitä monissa tietokoneohjelmissa ja niiden generointi on tärkeä osa ohjelmointia.

## Miten
Voit generoida satunnaisia numeroita Pythonilla käyttämällä sisäänrakennettua "random" -kirjastoa. Voit aloittaa tuomalla tämän kirjaston käyttämällä "import random" -komentoa. Sitten voit käyttää "random.randint(a, b)" -funktiota, jossa a ja b ovat ala- ja ylärajat halutulle numeroalueelle. Esimerkki:

```Python
import random
luku = random.randint(1, 10)
print(luku)
```
Output:
```
7
```
## Syvällinen sukellus
Random-kirjasto käyttää algoritmeja ja näin ollen se ei ole täysin satunnainen, vaan tuottaa pseudosatunnaisia numeroita. Tämä tarkoittaa, että sattumanvaraisuudesta huolimatta saman koodin tuottamat numerot toistuvat samassa järjestyksessä, joten esimerkiksi numero 42 ei välttämättä ole koskaan mahdollinen tulos. Jos haluat, voit myös määrittää tietyn siemennumeron, jolloin koodi tuottaa aina samat satunnaiset numerot joka kerta.

## Katso myös
- [Pythonin virallinen dokumentaatio random-kirjastosta](https://docs.python.org/3/library/random.html)
- [Satunnaislukugeneraattori artikkelissa "Ohjaus ja satunnaisuus"](https://www.teach.cs.toronto.edu/~csc104h/summer/slides/week14/Randomness.pdf)
- [Satunnaislukujen käyttö testeissä ja ohjelmoinnin laadun varmistamisessa](https://ieeexplore.ieee.org/document/5377899)