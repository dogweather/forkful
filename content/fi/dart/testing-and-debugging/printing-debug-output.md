---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.571964-07:00
description: "Kuinka: Dartissa voit tulostaa debug-tulosteen k\xE4ytt\xE4m\xE4ll\xE4\
  \ `print()`-funktiota. N\xE4in voit tulostaa yksinkertaisia viestej\xE4 ja muuttujien\
  \ arvoja."
lastmod: '2024-03-13T22:44:56.273552-06:00'
model: gpt-4-0125-preview
summary: "Dartissa voit tulostaa debug-tulosteen k\xE4ytt\xE4m\xE4ll\xE4 `print()`-funktiota."
title: Debug-tulosteen tulostaminen
weight: 33
---

## Kuinka:
Dartissa voit tulostaa debug-tulosteen käyttämällä `print()`-funktiota. Näin voit tulostaa yksinkertaisia viestejä ja muuttujien arvoja:

```dart
void main() {
  String tervehdys = "Hei, Dart!";
  print(tervehdys); // Tulostaa: Hei, Dart!

  int numero = 42;
  print('Numero on $numero.'); // Tulostaa: Numero on 42.
}
```

Rakenteellisten tietojen, kuten listojen tai olioiden, kohdalla Dartin `toString()`-metodi ei ehkä tarjoa tarpeeksi yksityiskohtia. Näissä tapauksissa voit käyttää `jsonEncode`-funktiota Dart `dart:convert`-kirjastosta muuntaaksesi tiedot JSON-merkkijonoksi luettavampaa tulostetta varten:

```dart
import 'dart:convert';

void main() {
  var kayttaja = {
    'nimi': 'John Doe',
    'ika': 30,
    'sahkopostit': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(kayttaja));
  // Tulostaa: {"nimi":"John Doe","ika":30,"sahkopostit":["john.doe@example.com","john@example.com"]}
}
```

Kun tarvitaan edistyneempiä virheenjäljityskykyjä, kuten loggausta eri tärkeysasteilla (info, varoitus, virhe), voit käyttää kolmannen osapuolen kirjastoja, kuten `logger`. Näin voit käyttää sitä:

1. Lisää `logger` `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Käytä `logger`-kirjastoa Dart-koodissasi:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Tämä on debug-viesti");
  logger.w("Tämä on varoitusviesti");
  logger.e("Tämä on virheviesti");
}
```

Tuloste on informatiivisempi, näyttäen viestin tason ja itse viestin, mikä tekee erilaisten logiviestien erottamisesta helpompaa.
