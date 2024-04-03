---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:17.671407-07:00
description: "Loggen in Dart verwijst naar het proces van het vastleggen van informatie\
  \ op verschillende niveaus tijdens de uitvoering van een programma. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.512242-06:00'
model: gpt-4-0125-preview
summary: Loggen in Dart verwijst naar het proces van het vastleggen van informatie
  op verschillende niveaus tijdens de uitvoering van een programma.
title: Loggen
weight: 17
---

## Hoe te:
Dart bevat een eenvoudig logmechanisme via de `dart:developer` bibliotheek. Voor meer geavanceerde logbehoeften wenden programmeurs zich vaak tot externe bibliotheken zoals `logger` en `log4dart`.

### Gebruikmakend van `dart:developer`
Dit is geschikt voor basislogging, vooral tijdens ontwikkeling:

```dart
import 'dart:developer';

void main() {
  log('Dit is een debug logbericht.');
}
```

Output:
```
Dit is een debug logbericht.
```

### Gebruikmakend van het `logger` pakket
Voor een uitgebreidere oplossing biedt het `logger` pakket verschillende niveaus van loggen (bijv. info, waarschuwing, fout) en kan op een leesbaardere wijze worden geformatteerd.

Voeg eerst de `logger` afhankelijkheid toe in je `pubspec.yaml` bestand:

```yaml
dependencies:
  logger: ^1.0.0
```

Gebruik het vervolgens als volgt:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Dit is een debugbericht");
  logger.w("Dit is een waarschuwingsbericht");
  logger.e("Dit is een foutbericht");
}
```

Een voorbeelduitvoer zou er zo uit kunnen zien, met elk berichttype dat anders is geformatteerd voor gemakkelijke identificatie:

```
💬 Dit is een debugbericht
⚠️ Dit is een waarschuwingsbericht
❗️ Dit is een foutbericht
```

### Gebruikmakend van het `log4dart` pakket
Voor applicaties die configuratie-gebaseerd loggen vereisen (vergelijkbaar met Log4j), biedt `log4dart` een bekende aanpak. Het is vooral handig voor grootschalige applicaties.

Zorg dat je `log4dart` opneemt in je `pubspec.yaml`:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Een eenvoudig gebruiksvoorbeeld:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Debuggen van MyApp");
  logger.info("Informatief bericht");
}
```

Output:

```
DEBUG: Debuggen van MyApp
INFO: Informatief bericht
```

Elk van deze methoden biedt een ander niveau van flexibiliteit en complexiteit, van eenvoudige debugberichten tot uitgebreide, configureerbare logregistratie die afgestemd is op de behoeften van complexe applicaties.
