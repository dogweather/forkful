---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:06.780593-07:00
description: "Dartissa lokitus viittaa ohjelman suorituksen aikana tapahtuvaan eri\
  \ tasojen tiedon tallentamiseen. Ohjelmoijat tekev\xE4t sen seuratakseen ohjelmiston\u2026"
lastmod: '2024-03-13T22:44:56.277687-06:00'
model: gpt-4-0125-preview
summary: Dartissa lokitus viittaa ohjelman suorituksen aikana tapahtuvaan eri tasojen
  tiedon tallentamiseen.
title: "Lokitiedostojen k\xE4sittely"
weight: 17
---

## Mikä ja miksi?

Dartissa lokitus viittaa ohjelman suorituksen aikana tapahtuvaan eri tasojen tiedon tallentamiseen. Ohjelmoijat tekevät sen seuratakseen ohjelmiston käyttäytymistä, debugataakseen ongelmia ja analysoidakseen suorituskykyä, mikä tekee sovelluksen ylläpidosta ja parantamisesta ajan myötä helpompaa.

## Kuinka:

Dart sisältää yksinkertaisen lokitusmekanismin `dart:developer` kirjaston kautta. Monimutkaisempien lokitustarpeiden osalta ohjelmoijat kääntyvät usein kolmansien osapuolien kirjastojen, kuten `logger` ja `log4dart`, puoleen.

### Käyttäen `dart:developer`
Tämä sopii peruslokittamiseen, erityisesti kehityksen aikana:

```dart
import 'dart:developer';

void main() {
  log('Tämä on debug-lokiviesti.');
}
```

Tuloste:
```
Tämä on debug-lokiviesti.
```

### Käyttäen `logger`-pakettia
Kattavampaa ratkaisua varten `logger`-paketti tarjoaa eri tasoja lokittamiselle (esim. info, varoitus, virhe) ja sen voi muotoilla luettavammalla tavalla.

Lisää ensin `logger`-riippuvuus `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  logger: ^1.0.0
```

Käytä sitten seuraavasti:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Tämä on debug-viesti");
  logger.w("Tämä on varoitusviesti");
  logger.e("Tämä on virheviesti");
}
```

Esimerkkituloste voisi näyttää tältä, jossa jokainen viestityyppi on muotoiltu eri tavoin helposti tunnistettavaksi:

```
💬 Tämä on debug-viesti
⚠️ Tämä on varoitusviesti
❗️ Tämä on virheviesti
```

### Käyttäen `log4dart`-pakettia
Sovelluksille, jotka vaativat konfiguraatioon perustuvaa lokitusta (samankaltainen kuin Log4j), `log4dart` tarjoaa tutun lähestymistavan. Se on erityisen kätevä suurille sovelluksille.

Varmista, että sisällytät `log4dart` `pubspec.yaml`-tiedostossasi:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Yksinkertainen käyttöesimerkki:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Debuggaan MyAppia");
  logger.info("Informaatioviesti");
}
```

Tuloste:

```
DEBUG: Debuggaan MyAppia
INFO: Informaatioviesti
```

Jokainen näistä menetelmistä tarjoaa eri tason joustavuutta ja monimutkaisuutta, yksinkertaisista debug-viesteistä kattaviin, konfiguroitaviin lokituksiin, jotka soveltuvat monimutkaisten sovellusten tarpeisiin.
