---
title:                "Väliaikaistiedoston luominen"
html_title:           "Python: Väliaikaistiedoston luominen"
simple_title:         "Väliaikaistiedoston luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Väliaikaisten tiedostojen luominen on yksi tapa, jolla ohjelmoijat voivat työskennellä tiedostojen kanssa väliaikaisesti. Tämä tarkoittaa, että tiedosto on luotu vain tilapäisesti ja se poistetaan automaattisesti käytön jälkeen. Tätä tekniikkaa käytetään usein esimerkiksi silloin, kun ohjelma tarvitsee tallentaa jonkinlaisia tietoja, mutta ei halua pysyvästi tallentaa niitä järjestelmään.

## Kuinka tehdä?

Väliaikaisen tiedoston luominen Pythonissa on yksinkertaista. Voit käyttää `tempfile`-kirjastoa ja sen `NamedTemporaryFile()`-funktiota. Tämän toiminnon avulla voit luoda väliaikaisen tiedoston ja siihen liittyvän tiedostonimen. Voit myös määrittää, haluatko kirjoittaa tiedostoon, lukea siitä tai molempia.

```python
import tempfile
# Luodaan väliaikainen tiedosto 
with tempfile.NamedTemporaryFile() as temp_file:
    print(temp_file.name)
    # Voit tehdä haluamiasi toimintoja tiedostolla
    # Tiedosto poistetaan automaattisesti
```

Tässä esimerkissä `temp_file`-muuttuja sisältää tiedostonimen, johon voit viitata halutessasi käyttää tiedostoa.

## Syvempi sukellus

Väliaikaisia tiedostoja on käytetty jo pitkään tietokoneohjelmoinnissa. Ne ovat erityisen hyödyllisiä silloin, kun ohjelma tarvitsee tallentaa dataa lyhyeksi ajaksi, mutta ei ole tarvetta säilyttää sitä pysyvästi. Tämän lisäksi, väliaikaiset tiedostot auttavat välttämään turhia tiedostoja järjestelmässä.

Vaikka Pythonin `tempfile`-kirjasto on yksinkertainen ja tehokas tapa luoda väliaikaisia tiedostoja, on myös muita vaihtoehtoja, kuten `os`-kirjaston `mkstemp()`-funktio.

Tiedostonimen ja tiedoston avaamisen lisäksi, `NamedTemporaryFile()`-funktio tarjoaa myös muita parametreja, joiden avulla voit tarkemmin määrittää haluamasi tiedoston ominaisuudet.

## Katso myös

- [Pythonin tempfile-dokumentaatio](https://docs.python.org/3/library/tempfile.html)
- [Ohjeet tiedostojen käsittelyyn Pythonilla](https://www.pythonforbeginners.com/files/working-with-files-in-python)