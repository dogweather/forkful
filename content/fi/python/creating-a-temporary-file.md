---
title:    "Python: Väliaikaisen tiedoston luominen"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Miksi 
 
Kuinka moni teistä on törmännyt tilanteeseen, jossa tarvitsee käyttää väliaikaista tiedostoa? Ehkä ohjelman täytyy tallentaa tietoja hetkeksi, mutta lopulta ne hävitään tai korvataan uusilla. Tässä blogipostissa käsittelemme kuinka luodaan väliaikainen tiedosto Python-ohjelmoinnissa ja miksi se on hyötyllistä. 

## Kuinka 

Väliaikaisen tiedoston luominen Pythonissa on helppoa. Voit käyttää `tempfile` -kirjastoa ja sen `TemporaryFile` -funktiota luodaksesi väliaikaisen tiedoston. Katso esimerkki alla: 

```Python 
import tempfile 

# Luodaan väliaikainen tiedosto
with tempfile.TemporaryFile() as temp:
  # Kirjoitetaan dataa tiedostoon
  temp.write(b"Tämä on väliaikainen tiedosto")
  # Siirrytään tiedoston alkuun
  temp.seek(0)
  # Luetaan tiedoston sisältö ja tulostetaan se
  print(temp.read())

# Tiedosto on nyt automaattisesti suljettu ja poistettu 
```

Tässä esimerkissä käytämme `tempfile.TemporaryFile()` -funktiota luomaan väliaikainen tiedosto, joka tallentaa bittijonon `b"Tämä on väliaikainen tiedosto"`. Käytämme sitten `seek(0)` -funktiota siirtyäksemme tiedoston alkuun ja `read()` -funktiota lukeaksemme tiedoston sisällön ja tulostamme sen näytölle. Lopuksi tiedosto suljetaan automaattisesti `with` -lauseen avulla ja se poistetaan. 

## Syvällisempi sukellus 

`tempfile` -kirjastossa on muitakin hyödyllisiä funktioita väliaikaisten tiedostojen luomiseen, kuten `NamedTemporaryFile()` joka luo nimetyn väliaikaisen tiedoston, `TemporaryDirectory()` joka luo väliaikaisen hakemiston ja `mktemp()` joka luo yksittäisen väliaikaisen tiedoston nimen.

Voit myös määrittää, että väliaikainen tiedosto poistetaan vasta kun ohjelma sulkeutuu, käyttämällä `delete=False` -parametria `TemporaryFile()` ja `NamedTemporaryFile()` funktioissa.

Väliaikaisia tiedostoja voi käyttää moniin eri tarkoituksiin, kuten tiedostojen tallentamiseen väliaikaisesti, testaamiseen, ja turvallisuussyistä, sillä ne poistuvat automaattisesti käytön jälkeen.

# Katso myös 

- [Pythonin virallinen tempfile-dokumentaatio](https://docs.python.org/3/library/tempfile.html)
- [RealPythonin opas väliaikaisten tiedostojen käyttämiseen Pythonissa](https://realpython.com/python-tempfile/) 
- [Codecademyn artikkeli väliaikaisista tiedostoista Pythonissa](https://www.codecademy.com/articles/temporary-files-in-python)