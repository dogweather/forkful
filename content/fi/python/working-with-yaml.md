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

## Mikä & Miksi?
YAML on tietojen muotoilukieli, joka on suunniteltu helpottamaan tietojen tallentamista ja jakamista ohjelmien välillä. Se on suosittu valinta ohjelmoijille, koska se on helppo lukea ja kirjoittaa, mikä auttaa parantamaan koodin luettavuutta ja ylläpidettävyyttä.

## Miten:
YAML-tietoja käsitellään käyttämällä sarjaa avaimia ja arvoja, joita erotetaan kaksoispisteillä ja sisennettyä merkkijonoa. Koodin lukeminen ja kirjoittaminen tapahtuu käyttämällä YAML-kirjastoa, joka tarjoaa hyödyllisiä toimintoja tiedon lukemiseen ja kirjoittamiseen. Katso alla olevat esimerkit, jotka näyttävät, miten YAMLia käytetään Pythonissa.

```Python
import yaml

# Luetaan YAML-tiedosto
with open("data.yml") as file:
  data = yaml.load(file, Loader=yaml.FullLoader)

# Tulostetaan tiedon arvo
print(data["avain"])

# Tallennetaan data uuteen YAML-tiedostoon
data["uusi_avain"] = "uusi arvo"

with open("uusi_data.yml", "w") as file:
  yaml.dump(data, file)
```

## Syvempi sukellus:
YAML kehitettiin vuonna 2001, ja se oli alun perin suunniteltu helpottamaan tietojen konfigurointia ja tallentamista. Se on korvannut aiemmin suositun XML-muotoilukielen monissa ohjelmissa. Vaikka jotkut ohjelmoijat edelleen käyttävät XML:ää, YAML on tullut suosittu vaihtoehto sen helppokäyttöisyyden vuoksi. Vaikka se on pääasiassa suunniteltu Pythonia varten, sitä tukevat myös monet muut ohjelmointikielet, kuten Java ja Ruby.

## Katso myös:
- [YAML.org](https://yaml.org/) - YAML-kielen virallinen verkkosivusto
- [Python YAML Library Documentation](https://pyyaml.org/wiki/PyYAMLDocumentation) - ohjeet YAML-kirjaston käytöstä Pythonissa
- [XML vs. YAML: Which is More Flexible for Programming?](https://keepify.com/technical/xml-vs-yaml-comparison) - vertailu XML:n ja YAML:n välillä