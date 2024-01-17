---
title:                "Satunnaislukujen luominen"
html_title:           "Python: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Satunnaislukujen luominen on tärkeä osa monia ohjelmointitehtäviä. Kuten nimi vihjaa, se tarkoittaa satunnaisten numeroiden luomista ohjelmoidusti. Tämä on hyödyllistä esimerkiksi pelikehityksessä, tietokantasovelluksissa tai testidataa luotaessa.

## Miten:
```Python
import random
print(random.random())
```
Tämä yksinkertainen koodinpätkä luo pseudosatunnaisen numeron väliltä 0 ja 1. Tämä voi olla hyödyllistä esimerkiksi pelin arvonnan tapahtumassa. Random-modulin avulla voit luoda myös satunnaisia kokonaislukuja tietyllä välillä, valita satunnaisesti listan elementtejä tai sekoittaa listan järjestyksen.

## Syväsukellus:
Satunnaislukujen luominen ei ole uusi keksintö, vaan jo antiikin ajoilta on löydetty menetelmiä satunnaisluvun luomiseen esimerkiksi noppien heittämisen avulla. Nykyään ohjelmoijille on tarjolla erilaisia algoritmeja ja generaattoreita satunnaislukujen luomiseen. On myös hyvä huomata, että tietokoneen generoimat satunnaisluvut ovat pääasiassa pseudosatunnaisia, eli ne seuraavat tiettyä laskennallista kaavaa eivätkä ole täysin sattumanvaraisia.

## Katso myös:
- [Pythonin virallinen dokumentaatio generoiden random-modulille](https://docs.python.org/3/library/random.html)
- [Satunnaislukugeneraattorit eri ohjelmointikielissä](https://blog.finxter.com/satunnaislukugeneraattorit-eri-ohjelmointikielissa/)
- [Satunnaislukujen käyttö pelikehityksessä](https://www.gamasutra.com/view/feature/130007/creating_randomness_in_games.php)