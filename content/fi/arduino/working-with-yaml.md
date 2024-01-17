---
title:                "Työskentely yaml:n kanssa"
html_title:           "Arduino: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mikä on YAML ja miksi ohjelmoijat käyttävät sitä?

YAML on ihmisen luettava tietokielto, jota käytetään tallentamaan ja jakamaan tietoa eri ohjelmistojen välillä. Se on yksinkertainen ja helppokäyttöinen muoto, joka helpottaa tiedonhallintaa ja tiedonsiirtoa ohjelmien välillä. Ohjelmoijat käyttävät YAML-kieltoa, koska se mahdollistaa tietojen organisoinnin ja tallentamisen käyttäen helposti ymmärrettävää syntaxia.

## Kuinka käyttää YAML:ia Arduino-ohjelmoinnissa

YAML-tiedostoja voidaan käyttää Arduino-ohjelmoinnissa mm. tallentamaan konfiguraatioasetuksia tai lukemaan antureiden lukemia. Alla on esimerkkejä YAML-kielen käytöstä Arduino-koodissa.

```
Arduino LiquidCrystal library#include <LiquidCrystal.h>

// Esimerkki YAML-tiedoston käytöstä LCD-näytön tekstin tallentamiseen
void setup() {
  YAML::Load("LCD_config.yml");
  LiquidCrystal lcd(12, 11, 5, 4, 3, 2);
  for (int i = 0; i < lines_to_write; i++) {
    lcd.write(lcd_text[i]);
  }
}
```

```
Arduino Serial library#struct Color {
  int red;
  int blue;
  int green;
};

// Esimerkki YAML-tiedoston lukemisesta ja sen tietojen tallentamisesta structiin
void setup() {
  YAML::Load("color_config.yml");
  Color main_color = YAML::Get<Color>("main_color");
  Serial.println("Main color values:");
  Serial.println(main_color.red);
  Serial.println(main_color.blue);
  Serial.println(main_color.green);
}
```

## Syvemmälle YAML-kieleen

YAML-kielen historian juuret juontavat juurensa vuoteen 2001, jolloin se kehitettiin Ruby-ohjelmointikielen yhteydessä. Se on saavuttanut suosiota erityisesti web-sovellusten, kuten Ruby on Railsin, käytössä. YAML-kieltoon on myös olemassa vaihtoehtoja, kuten JSON-kieli, mutta YAML-kielen yksinkertainen syntax ja ihmisläheinen formaatti ovat tehneet siitä suositun vaihtoehdon.

## Katso myös

[Official YAML Website](https://yaml.org/)

[YAML Tutorial for Beginners](https://www.guru99.com/yaml-tutorials.html)

[Comparison of YAML and JSON](https://yaml-multiline.info/)