---
title:                "Python: Väliaikaisen tiedoston luominen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Väliaikaisten tiedostojen luominen on ratkaisevan tärkeää monissa tilanteissa ohjelmoinnissa. Niitä käytetään esimerkiksi tallentamaan tilapäisiä tietoja tai tiedostoja, jotka voidaan poistaa turvallisesti sen jälkeen kun niitä ei enää tarvita.

# Miten luoda väliaikainen tiedosto

Väliaikaisen tiedoston luominen Pythonissa on helppoa käyttämällä `tempfile` -kirjastoa. Se tarjoaa erilaisia ​​toimintoja ja vaihtoehtoja väliaikaisten tiedostojen luomiseen ja hallintaan. Alla on esimerkkejä eri tapoista luoda väliaikainen tiedosto ja sen käyttämisestä:

```Python
import tempfile

# Luodaan väliaikainen tiedosto ja tallennetaan se muuttujaan
temp_file = tempfile.TemporaryFile()

# Kirjoitetaan tiedostoon tekstiä
temp_file.write(b"Tämä on väliaikainen tiedosto.")

# Siirrytään tiedoston alkuun
temp_file.seek(0)

# Luetaan tiedostosta ja tulostetaan sen sisältö
print(temp_file.read())

# Suljetaan ja poistetaan tiedosto automaattisesti
temp_file.close()

# Luodaan väliaikainen hakemisto ja sen alle tiedosto
with tempfile.TemporaryDirectory() as temp_dir:
    temp_path = tempfile.mkstemp(dir=temp_dir)[1]
    # tulostetaan luodun tiedoston polku
    print(temp_path)
```

## Tulostus

```
b'Tämä on väliaikainen tiedosto.'

/var/folders/hr/cf98j0l93xv1qrftlcswxgp00000gn/C/tmpttuohunz
```

# Syvemmälle väliaikaisiin tiedostoihin

Väliaikaisten tiedostojen luomisessa on useita tärkeitä asioita, jotka on otettava huomioon. Esimerkiksi `delete=False` asetus `mkstemp()` -toiminnossa estää tiedoston automaattisen poistamisen suljettaessa. `NamedTemporaryFile()` -toiminto luo nimetyn tiedoston, johon voidaan myöhemmin viitata uudelleen. `gettempdir()` palauttaa oletushakemiston, johon väliaikaisia tiedostoja tallennetaan.

Katso [Pythonin virallinen dokumentaatio](https://docs.python.org/fi/3/library/tempfile.html) lisätietoja ja vaihtoehtoja väliaikaisten tiedostojen luomiseen.

# Katso myös

- [How to manage temporary files in Python?](https://www.geeksforgeeks.org/how-to-manage-temporary-files-in-python/)
- [Creating Temporary Files and Directories with Python](https://stackabuse.com/creating-temporary-files-and-directories-with-python/)
- [Understanding Temporary Files and Folders in Python](https://levelup.gitconnected.com/understanding-temporary-files-and-folders-in-python-ae475e7c2b0)