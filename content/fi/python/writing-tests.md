---
title:                "Testien kirjoittaminen"
html_title:           "Python: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa ohjelmointia, sillä se auttaa varmistamaan koodin toimivuuden ja vähentää mahdollisten virheiden riskiä. Käyttäjiltä saadun palautteen mukaan, testien kirjoittaminen myös helpottaa koodin ylläpitoa ja parantaa sen luettavuutta.

## Kuinka

Testien kirjoittamisessa noudatetaan yleensä yksinkertaista kaavaa: määritellään funktio tai luokka, jota halutaan testata ja kirjoitetaan sen toimintoja vastaava testi. Tässä esimerkissä käytämme yksinkertaista funktiota, joka laskee kahden luvun summan:

```Python
def laske_summa(luku1, luku2):
    return luku1 + luku2

# Testataan funktiota
print(laske_summa(5, 10))
```

Tulostus: 15

Testin avulla voimme varmistaa, että funktio toimii halutulla tavalla. Voimme myös antaa testille syötteitä, jotka olisi hyvä käydä läpi, kuten esimerkiksi negatiivisia ja nolla-syötteitä:

```Python
print(laske_summa(-8, 6))
```

Tulostus: -2

## Syvempi sukellus

Testeissä on myös mahdollista käyttää erilaisia tapoja, kuten yksikkötestauksessa käytettäviä testikehyksiä, kuten `pytest` tai `unittest`. Näillä työkaluilla voidaan tehdä monipuolisempia testejä ja saada parempia raportteja. Lisäksi testien kirjoittamisessa on hyvä noudattaa hyviä käytäntöjä, kuten esimerkiksi yksinkertaisuuden periaatetta ja testien nimeämistapojen standardointia.

## Katso myös

- [Pythonin virallinen dokumentaatio testaamisesta](https://docs.python.org/fi/3/library/unittest.html)
- [Hyvät käytännöt testeissä](https://codeinstitute.net/blog/python-unit-testing-best-practices/)
- [Pytest-dokumentaatio](https://docs.pytest.org/en/stable/)