---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:04.115942-07:00
description: "Merkkijonon pituuden selvitt\xE4minen Dartissa tarkoittaa annetun merkkijonon\
  \ koodiyksik\xF6iden (k\xE4yt\xE4nn\xF6ss\xE4 merkkien m\xE4\xE4r\xE4 yksinkertaistettuna)\
  \ m\xE4\xE4r\xE4n\u2026"
lastmod: '2024-03-11T00:14:30.192156-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon pituuden selvitt\xE4minen Dartissa tarkoittaa annetun merkkijonon\
  \ koodiyksik\xF6iden (k\xE4yt\xE4nn\xF6ss\xE4 merkkien m\xE4\xE4r\xE4 yksinkertaistettuna)\
  \ m\xE4\xE4r\xE4n\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Merkkijonon pituuden selvittäminen Dartissa tarkoittaa annetun merkkijonon koodiyksiköiden (käytännössä merkkien määrä yksinkertaistettuna) määrän määrittämistä. Ohjelmoijat tekevät tämän manipuloidakseen merkkijonoja tarkemmin, esimerkiksi validoidakseen syötettä, katkaistakseen näytettävää tekstiä tai käsitellessään datamuotoja, joissa pituus on tärkeä (esim. protokollat, joissa viestit on etuliitetty pituudella).

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
