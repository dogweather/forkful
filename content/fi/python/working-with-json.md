---
title:                "Python: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON on yksi tärkeimmistä tiedonsiirtomuodoista Python-ohjelmoijille. Se on helppo käyttää ja helposti luettavissa sekä ihmisille että ohjelmille. JSON-tiedostot ovat myös kevyitä ja helppoja jakaa.

## Miten

JSON-tiedostojen luominen ja lukeminen Pythonissa on helppoa käyttäen `json` -kirjastoa. Tämä mahdollistaa muun muassa tiedon tallentamisen lista- tai sanakirjarakenteina.

```Python
import json

# JSON-tiedoston luominen
data = {
    "harrastukset": ["luontoretket", "soittaminen", "kirjoittaminen"],
    "ikä": 26,
    "paikkakunnat": ("Helsinki", "Tampere", "Turku")
}

with open("tiedosto.json", "w") as f:
    json.dump(data, f)

# Tiedon lukeminen JSON-tiedostosta
with open("tiedosto.json", "r") as f:
    data = json.load(f)

print(data)
# Output: {'harrastukset': ['luontoretket', 'soittaminen', 'kirjoittaminen'], 'ikä': 26, 'paikkakunnat': ['Helsinki', 'Tampere', 'Turku']}
```

## Syvyyksissä

JSON-muoto (JavaScript Object Notation) syntyi alun perin JavaScript-kielen kanssa, mutta nykyään se on yleisesti käytössä myös muissa ohjelmointikielissä. JSON on täysin tekstipohjainen, mikä tarkoittaa että sen päälle on helppo rakentaa sovelluksia. JSON-tiedoston rakenne on yksinkertainen ja looginen, mikä tekee sen lukemisesta ja muokkaamisesta helppoa.

JSON-viestit ovat myös helposti siirrettäviä internetissä, mikä tekee siitä hyödyllisen esimerkiksi web-sovelluksille. Usein palvelimet lähettävät dataa JSON-muodossa ja vastaavasti web-sovellukset pyytävät dataa JSON-muodossa.

## Katso myös

- [JSON Introduction](https://www.w3schools.com/js/js_json_intro.asp)
- [Python JSON -dokumentaatio](https://docs.python.org/3/library/json.html)