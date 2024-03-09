---
title:                "Lukujen pyöristäminen"
date:                  2024-03-08T21:56:47.024355-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Numeroiden pyöristäminen on prosessi, jossa numero säädetään lähimpään kokonaislukuun tai määriteltyyn määrään desimaalipaikkoja. Ohjelmoijat pyöristävät usein numeroita yksinkertaistaakseen laskelmia, parantaakseen luettavuutta tai valmistellakseen tietoja näyttöä varten, varmistaen johdonmukaisuuden ja selkeyden numeerisissa tulosteissa.

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
