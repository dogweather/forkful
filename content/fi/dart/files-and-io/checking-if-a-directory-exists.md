---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:01.764260-07:00
description: "Kuinka tehd\xE4: Dart k\xE4ytt\xE4\xE4 `dart:io` kirjastoa tiedostojen\
  \ ja hakemistojen k\xE4sittelyyn. T\xE4ss\xE4 on yksinkertainen tapa tarkistaa,\
  \ onko kansio olemassa."
lastmod: '2024-03-13T22:44:56.286929-06:00'
model: gpt-4-0125-preview
summary: "Dart k\xE4ytt\xE4\xE4 `dart:io` kirjastoa tiedostojen ja hakemistojen k\xE4\
  sittelyyn."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka tehdä:
Dart käyttää `dart:io` kirjastoa tiedostojen ja hakemistojen käsittelyyn. Tässä on yksinkertainen tapa tarkistaa, onko kansio olemassa:

```dart
import 'dart:io';

void main() {
  var directory = Directory('polku/kansioosi');

  if (directory.existsSync()) {
    print('Kansio on olemassa');
  } else {
    print('Kansiota ei ole olemassa');
  }
}
```
Esimerkkituloste, jos kansio on olemassa:
```
Kansio on olemassa
```

Tai, jos sitä ei ole:
```
Kansiota ei ole olemassa
```

Monimutkaisempien skenaarioiden käsittelyyn, kuten asynkroniseen tarkistamiseen tai kansion luomiseen jos sitä ei ole, voit käyttää seuraavaa lähestymistapaa:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('polku/kansioosi');

  // Tarkista asynkronisesti, onko kansio olemassa
  var exists = await directory.exists();
  if (exists) {
    print('Kansio on olemassa');
  } else {
    print('Kansiota ei ole olemassa, luodaan...');
    await directory.create(); // Tämä luo kansion
    print('Kansio luotu');
  }
}
```

Esimerkkituloste, jos kansiota ei ollut olemassa ja se luotiin:
```
Kansiota ei ole olemassa, luodaan...
Kansio luotu
```

Dartin sisäänrakennetut kyvyt ovat yleensä riittäviä tiedostojen ja hakemistojen käsittelyyn, joten kolmannen osapuolen kirjastoja ei tyypillisesti tarvita tähän tehtävään. Kuitenkin, monimutkaisempiin tiedostojärjestelmän operaatioihin, paketit kuten `path` (polkujen käsittelyyn alustariippumattomalla tavalla) voivat täydentää `dart:io` kirjastoa, mutta eivät suoraan tarjoa monimutkaisempia hakemistojen olemassaolon tarkistuksia kuin mitä on näytetty.
