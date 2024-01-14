---
title:                "Python: Komentoriviparametrien lukeminen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Pythonilla ohjelmointia opettavat artikkelit usein sisältävät ohjeita, joiden avulla voit syöttää erilaisia arvoja koodiisi. Tiedät ehkä jo, miten syötät arvoja suoraan koodiin, mutta oletko koskaan miettinyt, miten voit syöttää arvoja koodiisi käyttämällä komentoriviparametrejä? Tässä blogikirjoituksessa opetan sinulle, miksi ja miten kannattaa lukea komentoriviparametreja Pythonissa.

## Kuinka
Pythonissa on sisäänrakennettu kirjasto, joka mahdollistaa komentoriviparametrien lukemisen. Tämä kirjasto on nimeltään "argparse". Alla olevassa esimerkissä näytän, kuinka voit lukea komentoriviparametreja ja käsitellä niitä yksinkertaisessa ohjelmassa.

```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--nimi", help="Anna nimesi")
parser.add_argument("--paiva", help="Anna syntymäpäiväsi")

args = parser.parse_args()

print("Hei {}, syntymäpäiväsi on {}".format(args.nimi, args.paiva))
```

Jos suoritat tämän koodin komentoriviltä antamalla "--nimi" ja "--paiva" komentoriviparametrit, voit nähdä, miten ne otetaan vastaan ja tulostetaan näytölle.

```sh
python ohjelma.py --nimi Liisa --paiva 5.10.
Hei Liisa, syntymäpäiväsi on 5.10.
```

Voit myös lisätä komentoriviparametreihin muita ominaisuuksia, kuten oletusarvoja, mahdollisia arvoja ja lyhyempiä vaihtoehtoja. Näitä ominaisuuksia voit tutkia lisää Pythonin argparse-dokumentaatiosta.

## Syvällinen tarkastelu
Nyt kun olet oppinut perusteet komentoriviparametrien lukemiseen Pythonissa, voit syventyä vieläkin enemmän. Huomaat ehkä, että saman ohjelman voi tehdä myös ilman "argparse"-kirjastoa ja lukemalla parametrit suoraan "sys.argv"-muuttujasta. Syvällisempi tarkastelu auttaa sinua ymmärtämään, miksi "argparse"-kirjastoa kannattaa käyttää ja mitä muita etuja se tarjoaa.

## Katso myös
- [Pythonin argparse-dokumentaatio](https://docs.python.org/3/library/argparse.html)
- [Johdatus Pythoniin - Komentoriviparametrit](https://www.learnpython.org/en/Command_Line_Arguments)
- [Pythonin sisäänrakennetun "sys" -moduulin dokumentaatio](https://docs.python.org/3/library/sys.html)