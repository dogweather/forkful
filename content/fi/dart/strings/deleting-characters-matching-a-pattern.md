---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:06.745853-07:00
description: "Kuinka: Dart tekee merkkien, jotka vastaavat ennalta m\xE4\xE4ritetty\xE4\
  \ kuviota, poistamisen suoraviivaiseksi k\xE4ytt\xE4m\xE4ll\xE4 s\xE4\xE4nn\xF6\
  llisi\xE4 lausekkeita ja\u2026"
lastmod: '2024-03-13T22:44:56.253099-06:00'
model: gpt-4-0125-preview
summary: "Dart tekee merkkien, jotka vastaavat ennalta m\xE4\xE4ritetty\xE4 kuviota,\
  \ poistamisen suoraviivaiseksi k\xE4ytt\xE4m\xE4ll\xE4 s\xE4\xE4nn\xF6llisi\xE4\
  \ lausekkeita ja `replaceAll`-metodia."
title: Mallin mukaisten merkkien poistaminen
weight: 5
---

## Kuinka:
Dart tekee merkkien, jotka vastaavat ennalta määritettyä kuviota, poistamisen suoraviivaiseksi käyttämällä säännöllisiä lausekkeita ja `replaceAll`-metodia. Peruskäyttöön ei vaadita kolmannen osapuolen kirjastoja, mikä tekee tästä lähestymistavasta erittäin saavutettavan.

Tässä on yksinkertainen esimerkki, joka demonstroi, kuinka poistaa numerot merkkijonosta:

```dart
void main() {
  String stringWithDigits = 'Dart123 on hauskaa456';
  // Määritä säännöllisen lausekkeen kuvio, joka vastaa kaikkia numeroita
  RegExp digitPattern = RegExp(r'\d');
  
  // Korvaa kaikki kuvion esiintymät tyhjällä merkkijonolla
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // Tuloste: Dart on hauskaa
}
```

Oletetaan, että käsittelet monimutkaisempaa skenaariota, kuten erikoismerkkien poistaminen lukuun ottamatta välilyöntejä ja välimerkkejä. Näin sen tekisit:

```dart
void main() {
  String messyString = 'Dart!@# on *&()hauskaa$%^';
  // Määritä kuvio, joka vastaa kaikkea paitsi kirjaimia, numeroita, välilyöntejä ja välimerkkejä
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // Tuloste: Dart! on hauskaa
}
```

Tehtäviin, jotka vaativat monimutkaisempaa kuvion vastaavuuden etsintää ja korvaamista, Dartin kattava `RegExp`-luokan dokumentaatio tarjoaa syväsukelluksen monimutkaisempiin ilmaisuihin ja niiden käyttöön. Kuitenkin yllä olevat esimerkit kattavat suurimman osan yleisistä käyttötarkoituksista Dart-ohjelmoinnissa, kun kyseessä on merkkien poistaminen kuvion perusteella.
