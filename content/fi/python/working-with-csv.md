---
title:                "Työskentely csv:n kanssa"
html_title:           "Python: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
CSV (Comma Separated Values) on yksinkertainen tapa tallentaa ja jakaa taulukkomuotoista dataa. CSV-tiedostot ovat yleensä luettavissa tekstieditorilla ja tietokoneohjelmilla, mikä tekee niistä helposti käytettävissä olevan vaihtoehdon moniin datan käsittelytehtäviin. Ohjelmoijat käyttävät CSV-tiedostoja usein, sillä ne tarjoavat helpon tavan siirtää tietoa eri ohjelmien välillä.

## Näin teet sen:
Python tarjoaa sisäänrakennetun moduulin, "csv", joka mahdollistaa CSV-tiedostojen käsittelyn. Katso alla oleva esimerkki, jossa tietoja luetaan tiedostosta ja tulostetaan näytölle:

```Python
import csv
with open('tiedosto.csv') as csv_tiedosto:
    lukija = csv.reader(csv_tiedosto)
    for rivi in lukija:
        print(rivi)
```

Tämä koodi lukee tiedostosta "tiedosto.csv" rivin kerrallaan ja tulostaa sen näytölle.

## Syvemmälle:
CSV-formaatti on kehitetty 1970-luvulla ja se on pysynyt suosittuna datan tallennusmuotona sen yksinkertaisuuden ja yhteensopivuuden vuoksi. Vaihtoehtoisia tapoja tallentaa taulukkomuotoista dataa ovat esimerkiksi JSON ja XML. CSV:n etuna näihin verrattuna on sen helppokäyttöisyys, mutta sen haittapuolena voi olla, että se ei tue monimutkaisempia tietorakenteita.

CSV-tiedoston käsittely Pythonilla on melko suoraviivaista, mutta on kuitenkin hyvä ottaa huomioon muutamia asioita. Esimerkiksi tiedostoa lukiessa täytyy ottaa huomioon mahdollinen rivinvaihtomerkki ja tekstin koodaustapa. CSV-moduuli tarjoaa vaihtoehtoja näiden käsittelyyn, joten hyvä dokumentaation lukeminen ennen käyttöä on suositeltavaa.

## Katso myös:
- [Pythonin virallinen dokumentaatio CSV-moduulista](https://docs.python.org/3/library/csv.html)
- [CSV-tiedostojen käsittelykurssi Codecademyltä](https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-files/cheatsheet)