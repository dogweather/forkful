---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:11.475931-07:00
description: "Die Erstellung einer tempor\xE4ren Datei in Dart beinhaltet das Generieren\
  \ einer Datei, die f\xFCr die kurzfristige Nutzung vorgesehen ist, haupts\xE4chlich\
  \ f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.602112-06:00'
model: gpt-4-0125-preview
summary: "Die Erstellung einer tempor\xE4ren Datei in Dart beinhaltet das Generieren\
  \ einer Datei, die f\xFCr die kurzfristige Nutzung vorgesehen ist, haupts\xE4chlich\
  \ f\xFCr Szenarien wie das Zwischenspeichern von Daten, tempor\xE4re Speicherung\
  \ zur Dateiverarbeitung oder das Halten von Informationen, die zu sensibel sind,\
  \ um sie lange zu behalten."
title: "Eine tempor\xE4re Datei erstellen"
weight: 21
---

## Wie:
Die `dart:io` Bibliothek von Dart erleichtert die Erstellung von temporären Dateien durch die `Directory` Klasse. Hier ist eine unkomplizierte Möglichkeit, eine temporäre Datei zu erstellen und einige Inhalte darin zu speichern:

```dart
import 'dart:io';

Future<void> main() async {
  // Erstellen eines temporären Verzeichnisses (systemspezifischer Standort)
  Directory tempDir = await Directory.systemTemp.createTemp('mein_temp_verzeichnis_');

  // Erstellen einer temporären Datei in diesem Verzeichnis
  File tempFile = File('${tempDir.path}/meine_temp_datei.txt');

  // Schreiben einiger Inhalte in die temporäre Datei
  await tempFile.writeAsString('Das ist einiger temporärer Inhalt');

  print('Temporäre Datei erstellt: ${tempFile.path}');

  // Beispiel-Ausgabe: Temporäre Datei erstellt: /tmp/mein_temp_verzeichnis_A1B2C3/meine_temp_datei.txt
}
```

### Verwendung einer Drittanbieter-Bibliothek: `path_provider`
Für Anwendungen (insbesondere Mobile-Apps mit Flutter) möchten Sie vielleicht temporäre Dateien auf eine einheitlichere und handhabbarere Weise erstellen. Das Paket `path_provider` kann Ihnen helfen, das richtige temporäre Verzeichnis über verschiedene Plattformen hinweg (iOS, Android usw.) zu finden.

Zuerst fügen Sie `path_provider` unter dependencies in Ihre `pubspec.yaml` hinzu:

```yaml
dependencies:
  path_provider: ^2.0.9
```

Und hier ist, wie Sie es verwenden können, um eine temporäre Datei zu erstellen:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Das temporäre Verzeichnis abrufen
  final Directory tempDir = await getTemporaryDirectory();

  // Eine temporäre Datei in diesem Verzeichnis erstellen
  final File tempFile = File('${tempDir.path}/meine_temp_datei.txt');

  // Einige Inhalte in die temporäre Datei schreiben
  await tempFile.writeAsString('Das ist einiger temporärer Inhalt mit path_provider');

  print('Temporäre Datei erstellt mit path_provider: ${tempFile.path}');

  // Beispiel-Ausgabe: Temporäre Datei erstellt mit path_provider: /tmp/meine_temp_datei.txt (Pfad kann je nach Plattform variieren)
}
```

Diese Snippets veranschaulichen die Erstellung und Interaktion mit temporären Dateien in Dart und bieten einen unkomplizierten und praktischen Ansatz für das Datenmanagement zu kurzfristigen Zwecken.
