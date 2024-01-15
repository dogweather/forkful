---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Python: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä osa Python-ohjelmointia, sillä se mahdollistaa käyttäjän antaman syötteen käsittelyn ja interaktiivisuuden ohjelman suorituksen aikana.

## Miten

Komentoriviparametrien lukeminen Pythonilla on helppoa. Se tapahtuu `sys` -kirjaston `argv` -listan avulla, joka sisältää syötetyt komentoriviparametrit. Alla on yksinkertainen esimerkki:

```Python
import sys

print(sys.argv)
```

Tämä koodi tulostaa kaikki annetut komentoriviparametrit listana. Esimerkiksi komennolla `python argumentit.py hello world` tulostus olisi seuraava:

```Python
['argumentit.py', 'hello', 'world']
```

Kuten nähdään, ensimmäinen alkio on aina itse tiedoston nimi ja sen jälkeen tulevat kaikki annetut parametrit.

Käyttäjän antamaa syötettä voi myös käsitellä eri tavoin, esimerkiksi tarkastamalla parametreja ja niiden määrää ennen niiden käyttämistä ohjelmassa.

## Syvällisempi sukellus

Komentoriviparametrien lukeminen Pythonilla mahdollistaa ohjelman käyttämisen interaktiivisemmin, sillä käyttäjä voi antaa tietoa ohjelmalle sen suorituksen aikana. Komennot voi myös antaa eri järjestyksessä ja niitä voidaan tarkastella ja käsitellä ohjelman sisällä.

On myös hyvä huomata, että `sys.argv` -lista sisältää aina merkkijonoja, joten mikäli ohjelmassa tarvitaan tietyn tyyppisitä arvoja, kuten kokonaislukuja tai liukulukuja, ne tulee muuttaa oikeaan muotoon ennen käyttämistä.

## Katso myös

- [Pythonin virallinen dokumentaatio komentoriviparametreista](https://docs.python.org/3/library/sys.html#sys.argv)
- [Stack Overflow -kysymys komentoriviparametrien lukemisesta Pythonilla](https://stackoverflow.com/questions/41683465/command-line-parameters-in-python)
- [Tutoriaali komentoriviparametrien lukemisesta Pythonilla](https://www.guru99.com/python-command-line-arguments.html)