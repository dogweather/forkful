---
title:                "Työskentely yaml:n kanssa"
html_title:           "Python: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen ja lukeminen tiedostoja JSON-muodossa on raskasta ja hankalaa. YAML-tiedostot tarjoavat helpon ja ihmislähtöisen tavan tallentaa ja lukea dataa. Tämä tekee YAML:stä erittäin hyödyllisen työkalun Python-ohjelmointikielen käyttäjille.

## Kuinka

YAML-kirjaston asentamiseksi käytä pip-komentoa seuraavasti:
```
pip install pyyaml
```

Yksinkertaisen YAML-tiedoston lukeminen ja sen sisältämän datan tallentaminen muuttujaan voidaan tehdä seuraavasti:
```Python
import yaml

# avaa tiedosto
with open("data.yaml", 'r') as f:
    # lue data
    data = yaml.load(f)
    
# tulosta data
print(data)
```

Tämän koodin tulosteena näkyy YAML-tiedoston sisältämä data. Samoin tiedoston luominen ja siihen datan tallentaminen on yhtä helppoa:
```Python
import yaml

# luo data-sanakirja
data = {
    'nimi': 'Maija Meikäläinen',
    'ikä': 30,
    'harrastukset': ['uinti', 'valokuvaus', 'lukeminen']
}

# tallenna data YAML-tiedostoon
with open("data.yaml", 'w') as f:
    yaml.dump(data, f)
```

Tämän koodin suorittamisen jälkeen löydät luomasi YAML-tiedoston, joka sisältää annetun datan.

## Syvemmälle

YAML-tiedostoja käytetään yleensä konfiguraatiotiedostoina ohjelmoinnissa. Ne tarjoavat helpon ja selkeän tavan määritellä asetuksia ja vaihtoehtoja ohjelmalle. YAML-tiedostot ovat myös hyödyllisiä silloin, kun halutaan tallentaa ja lukea muuttuvaa dataa, kuten käyttäjien syöttämiä tietoja.

YAML:n syntax on kevyt ja ihmislähtöinen. Se käyttää sisennyksiä erottaakseen tiedon loogisesti toisistaan, mikä tekee siitä helppolukuisen ja helppokäyttöisen. YAML-tiedostot ovat myös helposti luettavissa muille ohjelmointikielille, kuten JSON-muodossa.

## Katso myös

- [YAML-tutkimuskeskus](https://yaml.org/)
- [Ohjeet YAML-tiedostojen käsittelyyn Pythonilla](https://realpython.com/python-yaml/)