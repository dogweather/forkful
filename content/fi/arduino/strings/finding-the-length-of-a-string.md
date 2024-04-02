---
date: 2024-01-20 17:46:48.693442-07:00
description: "\"Stringin\" pituuden selvitt\xE4minen tarkoittaa merkkijonossa olevien\
  \ merkkien lukum\xE4\xE4r\xE4n laskemista. Ohjelmoijat tekev\xE4t t\xE4m\xE4n esimerkiksi\
  \ tietojen\u2026"
lastmod: '2024-03-13T22:44:56.816039-06:00'
model: gpt-4-1106-preview
summary: "\"Stringin\" pituuden selvitt\xE4minen tarkoittaa merkkijonossa olevien\
  \ merkkien lukum\xE4\xE4r\xE4n laskemista. Ohjelmoijat tekev\xE4t t\xE4m\xE4n esimerkiksi\
  \ tietojen\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

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
