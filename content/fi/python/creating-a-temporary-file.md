---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tilapäisen tiedoston luominen on prosessi, jossa ohjelmoija luo väliaikaisen tallennuspaikan tietokonejärjestelmäänsä. Ohjelmoijat tekevät sen usein välttääkseen tarpeettoman tallennustilan käytön ja parantaakseen ohjelman suorituskykyä.

## Näin teet:
Python's `tempfile`-moduulilla voidaan luoda väliaikaisia tiedostoja. Tässä on esimerkki:

```Python
import tempfile

# Luo tilapäinen tiedosto
temp = tempfile.TemporaryFile()

# Kirjoita jotain siihen
temp.write(b'Taman on testi text')
temp.seek(0)  # Siirry tiedoston alkuun

# Lue tiedosto
print(temp.read())
# Output: b'Taman on testi text'

# Sulje tiedosto, se poistetaan automaattisesti
temp.close()
```

## Syvällisempi tarkastelu
Historiallisesti ohjelmoijat luo manuaalisesti tilapäisiä tiedostoja, mutta riski on, että tiedostot jäävät järjestelmään, mikäli ne eivät poista niitä ohjelman suorituksen päätyttyä. Pythonin `tempfile`-moduuli ratkaisee tämän ongelman automatisoimalla tiedoston poistamisen, kun tiedostoon viitannut objekti ei ole enää käytössä.

Vaihtoehtoisesti voidaan käyttää `NamedTemporaryFile`-funktiota, jos tarvitaan tilapäinen tiedosto, jolla on nimi. Käytäntö on sama kuin `TemporaryFile`-funktion kanssa, vain tiedostonimi on tarjolla.

Moduuli luottaa alhaisen tason toteutusyksityiskohtiin, kuten `os`-moduuliin ja `O_TMPFILE`-lippuun Linuxissa, joilla tiedosto voidaan luoda ja pitää se salassa muilta prosesseilta.

## Katso myös
[Dokumentaatio: Python tempfile](https://docs.python.org/3/library/tempfile.html)

[Keskustelu: Stack Overflow 'Python, how to create a temporary file?'](https://stackoverflow.com/questions/15169101/how-to-create-a-temp-file-in-python)

[Tutorial: Real Python 'Working With Files in Python'](https://realpython.com/working-with-files-in-python/)