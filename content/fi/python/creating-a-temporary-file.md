---
title:                "Python: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Väliaikaisia tiedostoja käytetään useisiin tarkoituksiin Python-ohjelmoinnissa. Ne voivat auttaa tallentamaan väliaikaisia tietoja, jotka eivät ole tarpeen pitkäaikaiseen tallennukseen tai ne voivat auttaa suorittamaan tiettyjä tehtäviä, kuten tiedostojen jakamista tai päivittämistä, ohjelman suorituksen aikana.

# Kuinka tehdä se?

Väliaikaisen tiedoston luominen Pythonissa on helppoa ja nopeaa. Käytämme 'tempfile' moduulia tämän tarkoitukseen. Voit aloittaa tuomalla tämän moduulin käyttäen seuraavaa koodia:

```Python
import tempfile
```

Arvailun välttämiseksi voimme määrittää väliaikaisen tiedoston nimen ja sijainnin käyttäen ```tempfile.NamedTemporaryFile()``` -funktiota. Tämä luo väliaikaisen tiedoston ja palauttaa sen tiedostokahvan, jota voimme käyttää työskennellessämme tiedoston kanssa.

```Python
temp_file = tempfile.NamedTemporaryFile()
print(temp_file.name)
```

Tämän koodin tulostuksena pitäisi näkyä väliaikaisen tiedoston nimi, esimerkiksi "/tmp/tmpxyz45tw".

# Syventävä tarkastelu

Tempfile-moduuli tarjoaa myös muita vaihtoehtoja väliaikaisten tiedostojen luomiseen. Voimme esimerkiksi määrittää tiedoston nimen ja sijainnin itse käyttäen ```tempfile.TemporaryDirectory()``` -funktiota.

```Python
temp_dir = tempfile.TemporaryDirectory()
print(temp_dir.name)
```

Tämä koodi tulostaa hakemiston nimen, joka on luotu väliaikaisia tiedostoja varten, esimerkiksi "/tmp/tmpxyz45tw".

Voimme myös asettaa väliaikaisen tiedoston poistettavaksi automaattisesti ohjelman suorituksen päätyttyä käyttämällä ```with``` -lauseketta.

```Python
with tempfile.TemporaryDirectory() as temp_dir:
    print(temp_dir.name)
```

Tämä varmistaa, että väliaikainen tiedosto poistetaan ohjelman suorituksen päätyttyä, vähentäen tarvetta huolehtia tiedoston poistamisesta itse.

# Katso myös

- [Tempfile-moduulin dokumentaatio](https://docs.python.org/3/library/tempfile.html)
- [Python-tiedostojen hallinta](https://realpython.com/python-file-management/)
- [Tietoa Pythonista](https://www.python.org/)