---
title:                "Tilapäistiedoston luominen"
html_title:           "Python: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary-tiedostojen luominen on hyödyllinen tapa käsitellä tiedostoja väliaikaisesti ilman, että niitä tarvitsee tallentaa pysyvästi tietokoneelle. Tämä on erityisen hyödyllistä silloin, kun työskentelet tiedostojen kanssa, jotka eivät tarvitse pysyvää tallennusta tai jotka eivät ole tärkeitä pitkäaikaisessa käytössä.

## Miten

```Python
import tempfile

# Luodaan väliaikainen tiedosto
with tempfile.TemporaryFile() as temp:
    # Kirjoitetaan tiedostoon tietoa
    temp.write(b"Tervetuloa lukemaan Python-ohjelmoijan artikkelia.")

    # Siirrytään tiedoston alkuun
    temp.seek(0)

    # Luetaan tiedoston sisältö ja tulostetaan se
    print(temp.read().decode('UTF-8'))

# Tiedosto on poistettu automaattisesti, kun with-lauseke päättyy
```

Tulostus: Tervetuloa lukemaan Python-ohjelmoijan artikkelia.

## Syventyvä tarkastelu

Pythonin `tempfile`-moduulissa on useita erilaisia toimintoja väliaikaisten tiedostojen luomiseen ja hallintaan. `TemporaryFile`-funktion lisäksi voit käyttää myös `NamedTemporaryFile`, joka antaa tiedostolle haluamasi nimen, tai `SpooledTemporaryFile`, joka säilyttää tiedoston sisällön muistissa ennen kuin se tallennetaan. Voit myös määrittää, haluatko tiedoston luotavan teksti- tai tavumerkkijonona. Lisätietoja löytyy Pythonin virallisesta dokumentaatiosta.

## Katso myös

- [Pythonin `tempfile`-moduulin virallinen dokumentaatio](https://docs.python.org/3/library/tempfile.html)
- [Pythonin `with`-lauseke](https://docs.python.org/3/reference/compound_stmts.html#with)