---
title:    "Python: Satunnaisten numeroiden luominen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaisnumeroiden generointi on tärkeä taito monissa ohjelmointitehtävissä, kuten pelituotannossa, tilastotieteessä ja salausalgoritmeissa. Se antaa mahdollisuuden luoda ennustamattomuutta ja monipuolisuutta koodiin.

## Miten

```Python
import random

# Generoi satunnainen kokonaisluku välillä 1-10
print(random.randint(1, 10))

# Generoi satunnainen desimaaliluku välillä 0-1
print(random.random())

# Generoi satunnainen kokonaisluku välillä 1-100 jokaisen loopin kierroksella
for i in range(10):
    print(random.randrange(1, 100))
```

```
Output:
6
0.7864535567
76
64
12
43
89
54
97
35
24
```

## Syvällinen sukellus

Pythonin `random`-moduuli tarjoaa monipuoliset mahdollisuudet satunnaisnumeroiden generointiin. Moduulin sisältämien erilaisten funktioiden avulla voit muokata generoitujen numeroiden välisiä rajoja ja tarkkuutta. Lisäksi `random` mahdollistaa myös "semirandom" numeroiden generoinnin, eli numeroiden generoinnin tietyn kaavan mukaan.

On tärkeää huomata, että satunnaisilla numeroilla ei ole muistia, joten jokaisen kerran generoidut numerot ovat täysin arvaamattomia ja riippumattomia aiemmista generoinneista.

## Katso myös

- [Pythonin `random`-moduulin virallinen dokumentaatio](https://docs.python.org/3/library/random.html)
- [Artikkeli: "Intro to Python Random Module"](https://realpython.com/python-random/)
- [Ohjelmointitehtävä: "Guess the Number Game"](https://www.practicepython.org/exercise/2014/04/02/09-guessing-game-one.html)