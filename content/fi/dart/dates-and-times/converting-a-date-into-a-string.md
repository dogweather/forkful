---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:58.234697-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi Dart-kieless\xE4\
  \ on yleinen teht\xE4v\xE4, kun tarvitsee n\xE4ytt\xE4\xE4 p\xE4iv\xE4m\xE4\xE4\
  r\xE4- ja aikatietoja ihmisluettavassa muodossa tai kun\u2026"
lastmod: '2024-03-13T22:44:56.283456-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi Dart-kieless\xE4 on\
  \ yleinen teht\xE4v\xE4, kun tarvitsee n\xE4ytt\xE4\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4\
  - ja aikatietoja ihmisluettavassa muodossa tai kun\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuttaminen merkkijonoksi"
weight: 28
---

## Mikä ja miksi?

Päivämäärän muuntaminen merkkijonoksi Dart-kielessä on yleinen tehtävä, kun tarvitsee näyttää päivämäärä- ja aikatietoja ihmisluettavassa muodossa tai kun aikoo sarjoittaa tietoja tallennusta tai siirtoa varten. Tämä prosessi mahdollistaa päivämäärä-aika-arvojen helpon esittämisen ja käsittelyn muodossa, joka on sekä ymmärrettävä että mukautettavissa käyttötarkoituksen mukaan.

## Kuinka:

Dart tarjoaa `DateTime`-luokan päivämäärien ja aikojen käsittelyyn sekä `intl`-paketin muotoiluun. Varmista ensin, että sinulla on `intl`-paketti lisäämällä `intl: ^0.17.0` (tai uusin versio) `pubspec.yaml`-tiedostoosi.

### Käyttäen Dart:n ydinkirjastoa

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Tuloste: 2023-4-12 (esimerkiksi, tämä riippuu nykyisestä päivämäärästä)
```

Tässä esimerkissä muodostetaan suoraan merkkijono `DateTime`-olion ominaisuuksista.

### Käyttäen `intl`-pakettia

Tuo ensin paketti:

```dart
import 'package:intl/intl.dart';
```

Sen jälkeen, muotoile päivämäärä:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Tuloste: 2023-04-12
```

`intl`-paketti mahdollistaa paljon monimutkaisemman muotoilun helposti, mukaan lukien paikalliset muodot:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Tuloste: huhtikuu 12, 2023
```

Nämä esimerkit osoittavat yksinkertaisia, mutta tehokkaita tapoja muuntaa ja muotoilla päivämääriä merkkijonoiksi Dartissa, joko käyttäen Dart:n ydintoimintoja tai hyödyntäen `intl`-pakettia monimutkaisempiin muotoiluvaihtoehtoihin.
