---
title:                "Tarkistetaan, onko hakemisto olemassa"
date:                  2024-03-08T21:54:01.764260-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistaminen, onko kansio olemassa Dartissa, liittyy kansion olemassaolon varmistamiseen määritetyssä polussa tiedostojärjestelmässä ennen tiedostojen lukemista tai kirjoittamista. Ohjelmoijat tekevät tämän välttääkseen virheitä, jotka tapahtuvat yrittäessä päästä käsiksi tai muokata olemattomia kansioita.

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
