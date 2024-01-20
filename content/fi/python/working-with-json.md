---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
JSON (JavaScript Object Notation) on helppolukuinen tiedonvaihtomuoto. Käytämme JSONia datan tallentamiseen ja verkon yli siirtämiseen koska se on kevyt ja kieliriippumaton.


## How to: (Kuinka tehdä:)
Pythonissa JSONin käsittely onnistuu `json` moduulilla. Lue ja kirjoita JSON dataa näin:

```Python
import json

# JSON dataa string-muodossa
json_data = '{"nimi": "Esa", "ika": 30, "onkoOhjelmoija": true}'

# Muunnetaan JSON data Python objektiksi (deserialisointi)
python_objekti = json.loads(json_data)
print(python_objekti['nimi'])  # Tulostaa: Esa

# Muunnetaan Python objekti JSON stringiksi (serialisointi)
uusi_json_data = json.dumps(python_objekti, indent=4)
print(uusi_json_data)
```


## Deep Dive (Syväsukellus)
JSON kehitettiin 2000-luvun alussa helpottamaan tiedonsiirtoa. XML oli aiemmin suosiossa, mutta JSON on selkeästi yksinkertaisempi ja nopeampi. JSONin käsittelyssä on tärkeää muistaa, että datatyypit ja merkintätavat saattavat vaihdella hieman eri ohjelmointikielissä. Pythonissa esimerkiksi `true` on `True`, ja `-` ei ole sallittu muuttujan nimessä.

Natiivin `json` moduulin lisäksi voit käyttää muita kirjastoja kuten `ujson` tai `simplejson` tarvittaessa nopeampaa serialisointia tai erikoisominaisuuksia varten.


## See Also (Katso Myös)
- Pythonin virallinen `json` moduulin dokumentaatio: https://docs.python.org/3/library/json.html
- JSONin virallinen määrittely: https://www.json.org/json-en.html
- W3Schools JSON opas: https://www.w3schools.com/js/js_json_intro.asp