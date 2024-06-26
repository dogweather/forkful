---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:58.234697-07:00
description: "Kuinka: Dart tarjoaa `DateTime`-luokan p\xE4iv\xE4m\xE4\xE4rien ja aikojen\
  \ k\xE4sittelyyn sek\xE4 `intl`-paketin muotoiluun. Varmista ensin, ett\xE4 sinulla\
  \ on `intl`-paketti\u2026"
lastmod: '2024-03-13T22:44:56.283456-06:00'
model: gpt-4-0125-preview
summary: "Dart tarjoaa `DateTime`-luokan p\xE4iv\xE4m\xE4\xE4rien ja aikojen k\xE4\
  sittelyyn sek\xE4 `intl`-paketin muotoiluun."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuttaminen merkkijonoksi"
weight: 28
---

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
