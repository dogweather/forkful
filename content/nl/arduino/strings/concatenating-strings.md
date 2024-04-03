---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:37.755504-07:00
description: "Het aaneenschakelen van strings betekent dat je ze eind-aan-eind samenvoegt\
  \ om een nieuwe te maken. Programmeurs doen dit om berichten samen te voegen,\u2026"
lastmod: '2024-03-13T22:44:51.064573-06:00'
model: gpt-4-0125-preview
summary: Het aaneenschakelen van strings betekent dat je ze eind-aan-eind samenvoegt
  om een nieuwe te maken.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
Laten we wat strings samenvoegen! Alles binnen de setup omdat we alleen snel willen kijken—geen herhalingslus nodig.

```arduino
void setup() {
  // Start seriële communicatie
  Serial.begin(9600);

  // Maak twee strings
  String begroeting = "Hallo, ";
  String naam = "Arduino!";

  // Voeg ze samen
  String gecombineerd = begroeting + naam;

  // Print het resultaat
  Serial.println(gecombineerd); 
}
void loop() {
  // Hier niets te herhalen
}
```

Je voert het uit en de uitvoer wacht op je in de Seriële Monitor:

```
Hallo, Arduino!
```

## Diepgaande Duik
Het aaneenschakelen van strings is al zo oud als de weg naar Rome in programmering—bestaat sinds de vroege talen hun eerste stappen zetten. In Arduino kun je of de `+` operator gebruiken zoals we deden, of de `+=` om een string aan een bestaande toe te voegen. Achter de schermen roepen deze operatoren eigenlijk functies aan die geheugenallocatie en het efficiënt kopiëren van de tekens afhandelen.

Waarom niet altijd samenvoegen? Nou, als je te maken hebt met kleine microcontrollers en veel string-samenvoegingen doet, kun je tegen geheugenproblemen aanlopen—omdat je elke keer dat je combineert, een nieuwe string maakt, waardoor meer geheugen wordt verbruikt. Voor zware stringmanipulatie grijpen mensen soms terug op karakterreeksen (klassieke C-stijl) om ruimte te besparen en potentiële prestatieproblemen te vermijden.

Bekijk ook stringfuncties zoals `concat()`, die niet alleen strings maar ook andere datatypen aan een bestaande string kan toevoegen.

## Zie Ook
Op zoek naar meer? Hier is waar je dieper kunt duiken:
- Arduino String Referentie: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Geheugenbeheer in Arduino: [learn.adafruit.com/memories-of-an-arduino](https://learn.adafruit.com/memories-of-an-arduino)
- Het Kwaad van Arduino Strings: [majenko.co.uk/blog/evils-arduino-strings](https://majenko.co.uk/blog/evils-arduino-strings)
