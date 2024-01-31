---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:46:48.693442-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
"Stringin" pituuden selvittäminen tarkoittaa merkkijonossa olevien merkkien lukumäärän laskemista. Ohjelmoijat tekevät tämän esimerkiksi tietojen validoinnin ja tekstipohjaisten komentojen käsittelyn yhteydessä.

## Näin teet:
```Arduino
void setup() {
  Serial.begin(9600); // Aloita sarjaviestintä nopeudella 9600 bps
}

void loop() {
  String data = "Moi Arduino!"; // Määritä merkkijono
  unsigned int length = data.length(); // Selvitä merkkijonon pituus

  Serial.print("Merkkijonon pituus: ");
  Serial.println(length); // Tulosta pituus

  delay(5000); // Viive ennen kuin toistetaan
}
```
Tuloste: `Merkkijonon pituus: 12`

## Syväsukellus
String-olioiden pituuden selvittäminen antaa ytimen String-luokan merkittäville toiminnoille, joka esiteltiin Arduino-ympäristöön helpottamaan merkkijonojen käsittelyä, joka oli aiemmin hankalaa C:n perusteella. Vaihtoehtoina on käyttää C-tyylisiä char-taulukoita ja funktioita, kuten `strlen()`, mutta ne ovat monimutkaisempia käsitellä ja virhealttiimpia. `String.length()` on turvallinen menetelmä, koska se kapseloi muistin hallinnan ja pituuden laskennan.

## Katso myös:
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
