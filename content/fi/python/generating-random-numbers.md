---
title:                "Python: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Python tarjoaa mahdollisuuden luoda satunnaisia numeroita moniin eri tarkoituksiin, kuten simulaatioihin, peleihin ja salausmenetelmiin. Luonnollisesti meidän tulee ensin oppia, miten näitä numeroita voi luoda.

## Kuinka tehdä se

Luodaksemme satunnaisia numeroita käytämme Pythonin `random`-moduulia. Ensin meidän tulee tuoda tämä moduuli käyttöömme:

```Python
import random
```

Luodessamme yksittäisen satunnaisen numeron käytämme `random.randint()`-funktiota. Tämä funktio ottaa parametreikseen kaksi kokonaislukua, `a` ja `b`, ja palauttaa satunnaisen kokonaisluvun väliltä `a` ja `b` mukaan lukien.

```Python
numero = random.randint(1, 10)
print(numero)
```

Tuloste:

```
5
```

Voimme myös luoda listan satunnaisia numeroita käyttäen `random.sample()`-funktiota. Tämä funktio ottaa parametreikseen listan ja halutun määrän satunnaisia numeroita. Se palauttaa listan, joka sisältää satunnaiset numerot halutussa järjestyksessä.

```Python
numerot = random.sample([1, 2, 3, 4, 5], 3)
print(numerot)
```

Tuloste:

```
[2, 5, 3]
```

Voimme myös käyttää `random.random()`-funktiota luodaksemme satunnaisen desimaaliluvun väliltä 0 (mukaan lukien) ja 1 (ei mukaan lukien).

```Python
desimaali = random.random()
print(desimaali)
```

Tuloste:

```
0.7309275634152
```

## Syventävä tieto

Pythonin `random`-moduuli käyttää Mersenne Twister -nimistä pseudosatunnaislukugeneraattoria luodessaan satunnaisia numeroita. Tämä on yksi yleisimmin käytetyistä pseudosatunnaislukugeneraattoreista, ja se perustuu monimutkaiseen matemaattiseen kaavaan.

On tärkeää ymmärtää, että nämä luodut numerot eivät ole täysin satunnaisia vaan perustuvat kaavaan. Tämän takia niitä ei tule käyttää esimerkiksi turvallisissa salauksissa.

## Katso myös

- [Pythonin virallinen dokumentaatio satunnaisista numeroista](https://docs.python.org/3/library/random.html)
- [Interaktiivinen harjoitus luoda satunnaisia numeroita Pythonilla](https://www.w3schools.com/python/ref_random_randint.asp)
- [Artikkeli satunnaisista numeroista ja niiden käytöstä datan analysoinnissa](https://www.datacamp.com/community/tutorials/numpy-random)