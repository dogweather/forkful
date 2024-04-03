---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:47.024355-07:00
description: "Kuinka: Dart tarjoaa alkuper\xE4isi\xE4 metodeja sen perus `num` tyypiss\xE4\
  \ py\xF6ristysoperaatioille. T\xE4ss\xE4 tutkimme metodeja kuten `round()`, `floor()`,\
  \ `ceil()`,\u2026"
lastmod: '2024-03-13T22:44:56.265109-06:00'
model: gpt-4-0125-preview
summary: "Dart tarjoaa alkuper\xE4isi\xE4 metodeja sen perus `num` tyypiss\xE4 py\xF6\
  ristysoperaatioille."
title: "Lukujen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Dart tarjoaa alkuperäisiä metodeja sen perus `num` tyypissä pyöristysoperaatioille. Tässä tutkimme metodeja kuten `round()`, `floor()`, `ceil()`, ja kuinka pyöristää tietty määrä desimaalipaikkoja.

### Pyöristäminen lähimpään kokonaislukuun:
```dart
var numero = 3.56;
print(numero.round()); // Tulostaa: 4
```

### Pyöristäminen alas:
```dart
print(numero.floor()); // Tulostaa: 3
```

### Pyöristäminen ylös:
```dart
print(numero.ceil()); // Tulostaa: 4
```

### Pyöristäminen tiettyyn määrään desimaalipaikkoja:
Pyöristääksemme tiettyyn määrään desimaalipaikkoja voimme käyttää `toStringAsFixed()` metodia, joka palauttaa merkkijonon, tai käyttää yhdistelmää `pow` `dart:math` kirjastosta saadaksemme numeerisen tuloksen.

```dart
import 'dart:math';

var numero = 3.56789;
String pyoristettyMerkkijono = numero.toStringAsFixed(2); // Näyttötarkoituksiin
print(pyoristettyMerkkijono); // Tulostaa: 3.57

double pyoristettyNumero = double.parse(pyoristettyMerkkijono);
print(pyoristettyNumero); // Tulostaa: 3.57

// Vaihtoehtoisesti, numeeriseen tulokseen:
double pyoristettyDesimaaleihin = (numero * pow(10, 2)).round().toDouble() / pow(10, 2);
print(pyoristettyDesimaaleihin); // Tulostaa: 3.57
```

Vaikka Dartin peruskirjasto kattaa suurimman osan pyöristystarpeista tehokkaasti, monimutkaisempiin matemaattisiin toimituksiin tai tarkkoihin pyöristysvaatimuksiin `decimal` kirjasto voi olla hyödyllinen. `Decimal` kirjasto tarjoaa helpon tavan työskennellä desimaalilukujen kanssa menettämättä tarkkuutta, mikä on erityisen kätevää taloudellisissa laskelmissa, mutta yksinkertaisiin pyöristysmenetelmiin, kuten esitetty, Dartin perustoiminnot ovat yleensä riittäviä.
