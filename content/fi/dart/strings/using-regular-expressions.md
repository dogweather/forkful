---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:23.975736-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) Dart-kieless\xE4 tarjoavat\
  \ tehokkaan tavan etsi\xE4 ja manipuloida merkkijonoja, mahdollistaen ohjelmoijien\
  \ suorittaa\u2026"
lastmod: '2024-03-13T22:44:56.259639-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) Dart-kieless\xE4 tarjoavat tehokkaan\
  \ tavan etsi\xE4 ja manipuloida merkkijonoja, mahdollistaen ohjelmoijien suorittaa\
  \ monimutkaisia tekstink\xE4sittelyteht\xE4vi\xE4 tehokkaasti."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Mikä & Miksi?
Säännölliset lausekkeet (regex) Dart-kielessä tarjoavat tehokkaan tavan etsiä ja manipuloida merkkijonoja, mahdollistaen ohjelmoijien suorittaa monimutkaisia tekstinkäsittelytehtäviä tehokkaasti. Regexin ymmärtämisen avulla kehittäjät voivat suorittaa tekstivalidointeja, etsiä kaavoja ja muuntaa tekstiä nopeasti, mikä on olennaista lomakkeiden käsittelyssä, datan jäsentämisessä ja yleisissä merkkijonojen manipulaatioissa moderneissa sovelluksissa.

## Miten:
Dart käyttää `RegExp`-luokkaa säännöllisiin lausekkeisiin. Tässä on perusesimerkki yksinkertaisen kaavan vastaamisesta merkkijonossa:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('Osuma löytyi!');
  } else {
    print('Ei osumia.');
  }
  // Tuloste: Osuma löytyi!
}
```

Osumien poimiminen merkkijonosta onnistuu `allMatches`-metodin avulla. Tämä metodi palauttaa iteroidun osumien kokoelman:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Tulostaa vastaavat alimerkkijonot.
  }
  // Tuloste:
  // Dart
  // is
  // awesome
}
```

Tekstin korvaaminen onnistuu käyttämällä `replaceFirst` tai `replaceAll` -metodeja:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // Korvaa ensimmäisen esiintymän
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Tuloste: Flutter is not just a dart.

  // Korvaa kaikki esiintymät
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Tuloste: Flutter is not just a flutter.
}
```

Merkkijonon jakaminen regex-kaavan mukaan on suoraviivaista käyttämällä `split`-metodia:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Vastaa mitä tahansa välilyöntimerkkiä
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // Tuloste: [Dart, is, fun]
}
```

Monimutkaisia jäsentämisiä tai valideja varten, joita Dartin `RegExp` ei suoraan tue, kannattaa harkita kolmannen osapuolen kirjastoja, mutta Dartin standardikirjasto on usein riittävä yleisimpiin regex-tehtäviin, korostaen sen hyödyllisyyttä ja monipuolisuutta säännöllisten lausekkeiden käsittelyssä.
