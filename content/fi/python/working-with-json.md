---
title:                "Työskentely jsonin kanssa"
html_title:           "Python: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-json.md"
---

{{< edit_this_page >}}

# Miksi

JSON-tiedostomuoto on yleisesti käytetty tapa tallentaa ja vaihtaa tietoja tietokonejärjestelmien välillä. Se on kevyt, luku- ja kirjoitusnopeudeltaan nopea ja ymmärrettävissä sekä ihmisille että koneille. JSON on myös yhteensopiva useiden ohjelmointikielten kanssa, minkä vuoksi sen käyttö on yleistä erilaisissa sovelluksissa ja projekteissa.

# Kuinka

JSON-tietomuoto koostuu avaimesta ja arvosta, jotka on erotettu kaksoispisteellä ja pilkulla. Avaimet ovat merkkijonoja ja arvot voivat olla mitä tahansa tietotyyppejä, kuten merkkijonoja, numeroita, boolean-arvoja tai jopa toisia JSON-objekteja tai -taulukoita.

```Python
# Esimerkki JSON-objektista, joka sisältää nimen ja iän
{
    "nimi": "Matti Meikäläinen",
    "ikä": 35
}
```

## Luku JSON-tiedostoja

JSON-tiedostoja voidaan lukea käyttämällä Pythonin built-in json-moduulia. Käytämme `json.load()` -funktiota lukeaksemme tiedoston ja palauttaaksemme sen Python-sanakirjana tai taulukkona.

```Python
import json

# Luetaan tiedosto ja tallennetaan se muuttujaan
with open("tiedosto.json", "r") as tiedosto:
    data = json.load(tiedosto)

# Tarkastellaan tiedoston sisältöä
print(data)
```

Output:

```
{'nimi': 'Matti Meikäläinen', 'ikä': 35}
```

## Kirjoita JSON-tiedostoja

Voimme myös luoda uuden JSON-tiedoston Pythonissa käyttämällä `json.dump()` -funktiota. Tämä ottaa parametrina tiedoston nimen ja tietorakenteen, jonka haluamme tallentaa tiedostoon.

```Python
import json

# Luodaan data-rakenne
data = {
    "nimi": "Matti Meikäläinen",
    "ikä": 35
}

# Tallennetaan tiedosto
with open("uusi_tiedosto.json", "w") as tiedosto:
    json.dump(data, tiedosto)
```

## Syväkellunta

JSON-tiedostot voivat myös sisältää monimutkaisempia rakenteita, kuten taulukoita ja sisäkkäisiä objekteja. Voimme käyttää JSON-moduulia päästäksemme näihin sisäisiin tietoihin ja käsitellä niitä.

```Python
import json

# Luetaan tiedosto
with open("tiedosto.json", "r") as tiedosto:
    data = json.load(tiedosto)

# Haetaan haluttu tieto
print(data["nimi"]) # Output: "Matti Meikäläinen"

# Käsitellään sisäkkäisiä tietoja
print(data["osoite"]["kaupunki"]) # Output: "Helsinki"
```

# Katso myös

- [Pythonin virallinen JSON-dokumentaatio](https://docs.python.org/3/library/json.html)
- [JSON-käsittely käytännössä - opas](https://realpython.com/python-json/)
- [JSON-formaatista lisätietoa W3Schoolsista](https://www.w3schools.com/js/js_json_intro.asp)