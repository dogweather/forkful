---
title:                "Python: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan työskennellyt tietokoneiden kanssa, olet varmasti törmännyt erilaisiin tiedostomuotoihin. Yksi näistä muodoista on YAML. Se on käytännöllinen tapa tallentaa tietoja rakenteellisesti, mikä tekee siitä hyödyllisen ohjelmoinnissa. Voit käyttää YAML:ia esimerkiksi konfigurointitiedostojen luomiseen tai tietojen tallentamiseen sovelluksissa. Jatka lukemista, jos haluat oppia lisää tästä hyödyllisestä tiedostomuodosta.

## Miten

YAML:n käyttäminen Pythonissa on helppoa. Sinun tarvitsee vain tuoda `yaml` kirjasto ja käyttää `load()` tai `dump()` funktiota lukeaksesi tai kirjoittaaksesi YAML-tiedostoja.

```Python
# Tuodaan yaml-kirjasto
import yaml

# Luodaan esimerkki tietoja
tiedot = {"nimi": "Maija",
          "ikä": 25,
          "työ": "Ohjelmoija"}

# Kirjoitetaan tiedot YAML-tiedostoon
with open("tiedosto.yaml", "w") as f:
    yaml.dump(tiedot, f)

# Luetaan YAML-tiedosto ja tallennetaan tiedot muuttujaan
with open("tiedosto.yaml", "r") as f:
    tiedot = yaml.load(f)

# Tulostetaan tiedot
print(tiedot)
# Output: {'nimi': 'Maija', 'ikä': 25, 'työ': 'Ohjelmoija'}
```

## Syvällisempi sukellus

YAML on tietojen rakenteistamisen kannalta hyödyllinen, koska se käyttää sisennyksiä ja aaltosulkumerkkejä erottamaan tietoja ja luomaan hierarkkisia rakenteita. Tämä tekee tiedostoista luettavia ja helposti muokattavia ihmisille.

YAML:lla on myös monia käteviä ominaisuuksia, kuten kyky tallentaa kuvaajia ja luoda ankkureita ja viitteitä tietojen välillä. Voit lukea lisää YAML:n ominaisuuksista ja syntaksista [täältä](https://yaml.org/spec/1.2/spec.html).

## Katso myös

- [Pythonin resurssikeskus Suomessa](https://python.fi/)
- [YAML:n asennusohjeet](https://yaml.org/start.html)
- [Pythonin YAML-dokumentaatio](https://pyyaml.org/wiki/PyYAMLDocumentation)