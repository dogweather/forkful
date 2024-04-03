---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:04.115942-07:00
description: "Kuinka: Dart tekee merkkijonon pituuden saamisesta suoraviivaista k\xE4\
  ytt\xE4en `length` -ominaisuutta. T\xE4ss\xE4 on perusesimerkki."
lastmod: '2024-03-13T22:44:56.260784-06:00'
model: gpt-4-0125-preview
summary: "Dart tekee merkkijonon pituuden saamisesta suoraviivaista k\xE4ytt\xE4en\
  \ `length` -ominaisuutta."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## Kuinka:
Dart tekee merkkijonon pituuden saamisesta suoraviivaista käyttäen `length` -ominaisuutta. Tässä on perusesimerkki:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("Merkkijonon '\(myString)' pituus on: \(myString.length)");
  // Tulostus: Merkkijonon 'Hello, Dart!' pituus on: 12
}
```
Tämä ominaisuus laskee merkkijonossa olevien UTF-16 koodiyksiköiden määrän, mikä vastaa merkkijonon pituutta useimmissa yleisissä käyttötapauksissa.

Hienostuneempaa tekstinkäsittelyä varten, erityisesti Unicode-merkkejä käsiteltäessä Basic Multilingual Plane (BMP) -tasosta poikkeavilla, harkitse `characters`-paketin käyttöä grafeemiklusterien laskemiseen, mikä edustaa tarkemmin käyttäjän havaitsemia merkkejä.

Lisää ensin `characters` `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  characters: ^1.2.0
```

Käytä sitten seuraavasti:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 perhe";
  print("Merkkijonon '\(myEmojiString)' pituus on: \(myEmojiString.characters.length)");
  // Tulostus: Merkkijonon '👨‍👩‍👧‍👦 perhe' pituus on: 8
}
```

Tässä esimerkissä `myEmojiString.characters.length` antaa meille pituuden Unicode-grafeemiklusterien määränä, mikä on tarkempi edustus merkkijonoille, jotka sisältävät monimutkaisia merkkejä, kuten emojit tai yhdistetyt merkkimerkit.
