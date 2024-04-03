---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:02.037450-07:00
description: "Virheilmoitusten kirjoittaminen standardivirhevirtaan (stderr) Dart-kieless\xE4\
  \ tarkoittaa virheviestien ja diagnostiikkatietojen l\xE4hett\xE4mist\xE4 erilliseen\u2026"
lastmod: '2024-03-13T22:44:56.289207-06:00'
model: gpt-4-0125-preview
summary: "Virheilmoitusten kirjoittaminen standardivirhevirtaan (stderr) Dart-kieless\xE4\
  \ tarkoittaa virheviestien ja diagnostiikkatietojen l\xE4hett\xE4mist\xE4 erilliseen\
  \ virtaan, joka eroaa tavallisesta tulostevirrasta (stdout)."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Mikä ja miksi?

Virheilmoitusten kirjoittaminen standardivirhevirtaan (stderr) Dart-kielessä tarkoittaa virheviestien ja diagnostiikkatietojen lähettämistä erilliseen virtaan, joka eroaa tavallisesta tulostevirrasta (stdout). Ohjelmoijat tekevät näin erottaakseen ohjelman normaalin tulosteen ja virhe- tai varoitusviestit, mikä mahdollistaa helpomman vianmäärityksen ja lokitiedostojen käsittelyn.

## Miten:

Dart-kielessä stderr-virtaan kirjoittaminen on suoraviivaista käyttämällä `dart:io`:ssa saatavilla olevaa `stderr`-objektia. Tässä on yksinkertainen esimerkki:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Tämä on virheviesti.');
}
```

Tuloste suoritettaessa:
```
Tämä on virheviesti.
```
Tämä viesti lähetetään stderr-virtaan, joka tyypillisesti näytetään konsolissa tai terminaalissa.

Jotta voimme demonstroida suurempaa monimutkaisuutta, kuten poikkeuksen lokitusta, Dartin rikas ominaisuusjoukko mahdollistaa tiiviin ja tehokkaan virheenkäsittelyn:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simuloidaan operaatio, joka saattaa heittää poikkeuksen
    throw Exception('Jotain meni pieleen!');
  } catch (e) {
    stderr.writeln('Virhe: $e');
  }
}

void main() {
  riskyOperation();
}
```

Tuloste suoritettaessa:
```
Virhe: Exception: Jotain meni pieleen!
```

Tämä malli on erityisen hyödyllinen sovelluksille, jotka tarvitsevat erottaa normaalit lokit virhelokeista, mikä tekee sovellusten seurannasta ja vianmäärityksestä helpompaa.

Vaikka Dartin standardikirjasto onkin melko kattava, monet ohjelmat eivät edellytä kolmannen osapuolen kirjastoja stderr-virtaan kirjoittamiseen. Kuitenkin, jos sovelluksesi tarvitsee monimutkaisempia lokitusominaisuuksia (esim. tiedostoihin, verkon yli, muotoilu), `logging`-paketti on suosittu valinta. Tässä on pikainen katsaus `logging`-paketin käyttöön virheiden yhteydessä:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Vakava virhe: Jotain merkittävän pahaa tapahtui.');
}
```

Tuloste suoritettaessa:
```
SEVERE: 2023-04-01 00:00:00.000: Vakava virhe: Jotain merkittävän pahaa tapahtui.
```

Tämä menetelmä tarjoaa suuremman määrän mukautusmahdollisuuksia ja hallintaa siitä, mikä lasketaan virheeksi ja miten se muotoillaan, mikä voi olla erittäin hyödyllistä suuremmissa, monimutkaisemmissa sovelluksissa.
