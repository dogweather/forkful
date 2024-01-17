---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Python: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Miksi ohjelmoijat haluavat muuttaa merkkijonoja pieniksi kirjaimiksi? Merkkijonon muuttaminen pieniksi kirjaimiksi on tärkeää, koska se tekee merkkijonoista yhtenäisen ja helposti vertailtavissa olevia. Tämä helpottaa esimerkiksi tietokantojen hakutoimintoja ja muuta tiedonkäsittelyä.

## Miten:

### Esimerkki 1:
```python
text = "HeLlO WoRlD"
lowercase_text = text.lower()
print(lowercase_text)
```
Tulostus:
```
hello world
```
Koodiesimerkissä käytetään `lower()`-metodia, joka muuttaa merkkijonon kaikki isot kirjaimet pieniksi kirjaimiksi. Tämän jälkeen lowercasemuuttujaan tallennetaan pieniksi kirjaimiksi muutettu merkkijono, joka tulostetaan lopuksi konsoliin.

### Esimerkki 2: 
```python
sentence = "I am learning Python!"
print(lowercase_text.count("python"))
```
Tulostus:
```
1
```
Tässä esimerkissä lowercasemuuttujaan tallennettu merkkijono `hello world` sisältää yhden esiintymän sanaa `python`. `count()`-metodi laskee annetun haetun merkkijonon esiintymät ja palauttaa lukumäärän.

## Deep Dive:
Historiallisessa kontekstissa, merkkijonon muuttaminen pieniksi kirjaimiksi on ollut tärkeää erityisesti eri ohjelmointikielissä, joissa isot ja pienet kirjaimet katsotaan erilaisiksi merkeiksi. Tämä on saattanut aiheuttaa vääriä hakutuloksia ja muita ongelmia koodissa. Myös merkkijonojen vertailussa on ollut tärkeää, että kaikki merkit ovat samassa muodossa.

On olemassa myös muita tapoja muuttaa merkkijono pieniksi kirjaimiksi. Esimerkiksi Pythonissa on `casefold()`-metodi, joka on hieman kattavampi kuin `lower()`-metodi, sillä se muuttaa myös kansainväliset merkit vastaaviin "isoihin" tai "pieniin" vastineisiinsa. Käytännössä tämä tarkoittaa, että vertailu on vielä luotettavampaa.

Pythonin merkkijonojen muuntaminen tapahtuu muuttamalla Unicode-koodipisteiden yläosia, eli alueita, joita tietyt merkit edustavat. Tämä tehdään Unicode-standardin avulla, joka on vakiintunut eri tietokoneiden ja ohjelmien väliseksi merkistöksi.

## Katso myös:
- [Pythonin string-muotoilu](https://docs.python.org/3/library/string.html)
- [Miksi ei ole hyvä idea käyttää lowercase-python-stringejä](https://miguelito.me/blog/2008/9/29/why-you-shouldnt-use-a-lowercase- python-string-lowercase-python-strings-are-just-fine)
- [Pythonin esittely Unicode-sivuilla](https://docs.python.org/3/howto/unicode.html)