---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:07.613357-07:00
description: "Das Protokollieren in Dart bezieht sich auf den Prozess der Aufzeichnung\
  \ verschiedener Informationsniveaus w\xE4hrend der Ausf\xFChrung eines Programms.\u2026"
lastmod: '2024-03-11T00:14:27.482363-06:00'
model: gpt-4-0125-preview
summary: "Das Protokollieren in Dart bezieht sich auf den Prozess der Aufzeichnung\
  \ verschiedener Informationsniveaus w\xE4hrend der Ausf\xFChrung eines Programms.\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?

Das Protokollieren in Dart bezieht sich auf den Prozess der Aufzeichnung verschiedener Informationsniveaus während der Ausführung eines Programms. Programmierer tun dies, um das Verhalten der Software zu überwachen, Probleme zu debuggen und die Leistung zu analysieren, was es einfacher macht, die Anwendung im Laufe der Zeit zu warten und zu verbessern.

## Wie geht das:

Dart umfasst einen einfachen Protokollierungsmechanismus durch die `dart:developer`-Bibliothek. Für anspruchsvollere Protokollierungsbedürfnisse wenden sich Programmierer oft an Drittanbieter-Bibliotheken wie `logger` und `log4dart`.

### Verwendung von `dart:developer`
Dies eignet sich für grundlegende Protokollierung, insbesondere während der Entwicklung:

```dart
import 'dart:developer';

void main() {
  log('Dies ist eine Debug-Protokollnachricht.');
}
```

Ausgabe:
```
Dies ist eine Debug-Protokollnachricht.
```

### Nutzung des `logger`-Pakets
Für eine umfassendere Lösung bietet das Paket `logger` verschiedene Protokollierungsstufen (z.B. Info, Warnung, Fehler) an und kann in einer besser lesbaren Art und Weise formatiert werden.

Fügen Sie zunächst die Abhängigkeit `logger` in Ihrer `pubspec.yaml`-Datei hinzu:

```yaml
dependencies:
  logger: ^1.0.0
```

Dann verwenden Sie es wie folgt:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Dies ist eine Debug-Nachricht");
  logger.w("Dies ist eine Warnungsnachricht");
  logger.e("Dies ist eine Fehlermeldung");
}
```

Eine beispielhafte Ausgabe könnte so aussehen, wobei jeder Nachrichtentyp unterschiedlich formatiert ist, um eine einfache Identifikation zu ermöglichen:

```
💬 Dies ist eine Debug-Nachricht
⚠️ Dies ist eine Warnungsnachricht
❗️ Dies ist eine Fehlermeldung
```

### Nutzung des `log4dart`-Pakets
Für Anwendungen, die eine konfigurationsbasierte Protokollierung benötigen (ähnlich wie Log4j), bietet `log4dart` einen vertrauten Ansatz. Es ist besonders praktisch für großangelegte Anwendungen.

Stellen Sie sicher, dass Sie `log4dart` in Ihre `pubspec.yaml` einschließen:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Ein einfaches Beispiel für die Nutzung:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MeineApp");
  logger.debug("Debugging von MeineApp");
  logger.info("Informationsnachricht");
}
```

Ausgabe:

```
DEBUG: Debugging von MeineApp
INFO: Informationsnachricht
```

Jede dieser Methoden bietet ein unterschiedliches Maß an Flexibilität und Komplexität, von einfachen Debug-Nachrichten bis hin zu umfassenden, konfigurierbaren Protokollierungen, die auf die Bedürfnisse komplexer Anwendungen zugeschnitten sind.
