---
title:                "Satunnaislukujen generointi"
html_title:           "Python: Satunnaislukujen generointi"
simple_title:         "Satunnaislukujen generointi"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaisten numeroiden luominen on tärkeä osa monia ohjelmointitehtäviä, kuten arpajaisten tai satunnaisen salasanan generointia. Se voi myös olla hyödyllistä testauksessa tai simuloinnissa.

## Miten

Satunnaisia numeroita voidaan luoda Pythonilla käyttämällä random-moduulia. Tässä on esimerkki satunnaisen kokonaisluvun generoinnista väliltä 1-10:

```Python
import random
luku = random.randint(1, 10)
print(luku)
```

Tämä koodi tulostaa satunnaisen kokonaisluvun väliltä 1-10 joka kerta, kun sitä suoritetaan.

Voit myös määrittää itse haluamasi alueen generoitaville numeroille. Tässä esimerkissä generoidaan satunnainen liukuluku väliltä 0-1:

```Python
import random
luku = random.random()
print(luku)
```

Voit myös saada listan satunnaisia numeroita käyttämällä random.sample-funktiota. Alla oleva esimerkki generoi 5 ainutlaatuista satunnaisnumeroa väliltä 1-10:

```Python
import random
lukuja = random.sample(range(1, 11), 5)
print(lukuja)
```

## Syventyvä tieto

Random-moduuli käyttää Mersenne Twister -algoritmia satunnaislukujen generointiin. Tämä algoritmi on yksi suosituimmista satunnaislukujen generointimenetelmistä ja se tuottaa laadukkaita ja tasaisesti jakautuneita satunnaislukuja.

On tärkeää huomata, että satunnaislukuja ei oikeasti voida luoda tietokoneessa täysin satunnaisesti, vaan ne perustuvat tietokoneen sisäisiin toimintoihin. Tämän vuoksi niitä ei tulisi käyttää salauksessa tai muissa turvallisuustarkoituksissa.

## Katso myös

- Täydellinen lista random-moduulin toiminnoista: https://docs.python.org/3/library/random.html
- Selitys Mersenne Twister -algoritmista: https://en.wikipedia.org/wiki/Mersenne_Twister