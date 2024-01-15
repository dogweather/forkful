---
title:                "Yamlin kanssa työskentely"
html_title:           "Arduino: Yamlin kanssa työskentely"
simple_title:         "Yamlin kanssa työskentely"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

# Miksi

Miksi kenenkään pitäisi kiinnostua YAML:n kanssa työskentelystä? Yksinkertaisesti sanottuna, YAML on käytännöllinen ja helppokäyttöinen tapa tallentaa ja jakaa tietoa tietokoneohjelmien kanssa. Se on erityisen hyödyllinen ohjelmointirobotiikan ja elektroniikan maailmassa, kuten Arduino.

# Kuinka

YAML:n käyttö Arduinoilla on helppoa ja suoraviivaista. Aloita lisäämällä tarvittavat kirjastot koodiisi.

```Arduino
#include <SPI.h>
#include <SD.h>
#include <YAMLCPP.h>
```

Seuraava vaihe on luoda YAML-tiedosto, joka sisältää tarvittavat tiedot. Voit käyttää esimerkiksi tekstieditoria tai YAML-editoria, kuten YAML-lint, luodaksesi YAML-tiedoston. Muista tallentaa se Arduino-projektin kansioon.

YAML-tiedoston luomisen jälkeen voit käyttää sitä koodissasi. Aloita avaamalla ja lukemalla YAML-tiedosto:

```Arduino
File configFile = SD.open("config.yml");
String yamlData = configFile.readString();
```

Sitten voit käyttää YAMLCPP-kirjastoa purkamaan YAML-tiedoston sisältö:

```Arduino
YAMLTree tree = YAML::Parse(yamlData);
```

Voit nyt käyttää tree-muuttujaa saatavilla olevien tietojen käsittelyyn. Esimerkiksi voit käyttää "get" -funktiota saadaksesi arvon tietystä avaimesta:

```Arduino
String name = tree.get("nimi").as<String>();
```

Lopuksi, voit käyttää näitä tietoja haluamallasi tavalla koodissasi, esimerkiksi näyttämällä ne LCD-näytöllä tai lähettämällä ne verkkoon.

# Deep Dive

YAML (Yet Another Markup Language) on ihmisen luettavissa oleva tiedostoformaatti, joka on suunniteltu tallentamaan ja jakamaan tietorakenteita ja -tietoja eri ohjelmistojen välillä. Se käyttää yksinkertaista ja intuitiivista syntaksia, joka muistuttaa paljon taulukkoa ja listaa.

YAML:n käyttö on yleistynyt erityisesti CI/CD (Continuous Integration/Continuous Delivery) -prosesseissa, mutta se soveltuu myös hyvin Arduino-ohjelmointiin. Se tarjoaa yksinkertaisen tavan tallentaa muuttujia ja tietorakenteita sekä tehokkaan tavan niiden käsittelyyn.

YAMLCPP-kirjasto Arduinoille tarjoaa kätevän tavan purkaa YAML-tiedostoja ja saada niistä tietoja käytettäväksi koodissa.

# Katso myös

- [YAML-lint](https://yamlint.com/) – Työkalu YAML-tiedostojen luomiseen ja validointiin.
- [YAMLCPP-kirjasto Arduinoille](https://github.com/jdevesa/YAMLCPP-Arduino) – Tarkempaa tietoa kirjaston toiminnasta ja käyttötavoista.
- [YAML-opas](https://yaml.org/) – Lisätietoa YAML:n rakenteesta, syntaksista ja käytöstä.