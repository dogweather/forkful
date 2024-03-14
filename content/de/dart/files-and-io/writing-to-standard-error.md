---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:55.287778-07:00
description: "Das Schreiben auf Standardfehler (stderr) in Dart geht darum, Fehlermeldungen\
  \ und Diagnostik in einen separaten Stream zu senden, der sich vom\u2026"
lastmod: '2024-03-13T22:44:53.599058-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben auf Standardfehler (stderr) in Dart geht darum, Fehlermeldungen\
  \ und Diagnostik in einen separaten Stream zu senden, der sich vom\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf Standardfehler (stderr) in Dart geht darum, Fehlermeldungen und Diagnostik in einen separaten Stream zu senden, der sich vom Standardausgang (stdout) unterscheidet. Programmierer tun dies, um zwischen normaler Programmausgabe und Fehlern oder Warnmeldungen zu unterscheiden, was das Debugging und das Loggen erleichtert.

## Wie geht das:

In Dart ist das Schreiben auf stderr unkompliziert mit dem `stderr`-Objekt möglich, das in `dart:io` verfügbar ist. Hier ist ein einfaches Beispiel:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Das ist eine Fehlermeldung.');
}
```

Ausgabe beim Ausführen:
```
Das ist eine Fehlermeldung.
```
Diese Nachricht wird an den stderr-Stream gesendet, der typischerweise in der Konsole oder im Terminal angezeigt wird.

Um mehr Komplexität zu demonstrieren, wie zum Beispiel das Protokollieren einer Ausnahme, ermöglicht Darts reicher Funktionsumfang eine prägnante und effektive Fehlerbehandlung:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Eine Operation simulieren, die einen Fehler auslösen könnte
    throw Exception('Etwas ist schief gelaufen!');
  } catch (e) {
    stderr.writeln('Fehler: $e');
  }
}

void main() {
  riskyOperation();
}
```

Ausgabe beim Ausführen:
```
Fehler: Exception: Etwas ist schief gelaufen!
```

Dieses Muster ist besonders nützlich für Anwendungen, die normale Logs von Fehlerlogs trennen müssen, was das Überwachen und Debuggen von Anwendungen erleichtert.

Obwohl Darts Standardbibliothek ziemlich umfassend ist, benötigen viele Programme keine Drittanbieterbibliotheken zum Schreiben auf stderr. Wenn Ihre Anwendung jedoch ausgefeiltere Logging-Fähigkeiten benötigt (z.B. in Dateien, über das Netzwerk, Formatierung), ist das `logging`-Paket eine beliebte Wahl. Hier ist ein kurzer Einblick in die Verwendung von `logging` für Fehler:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MeinAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Schwerer Fehler: Etwas bedeutend Schlimmes ist passiert.');
}
```

Ausgabe beim Ausführen:
```
SEVERE: 2023-04-01 00:00:00.000: Schwerer Fehler: Etwas bedeutend Schlimmes ist passiert.
```

Diese Methode bietet einen höheren Grad an Anpassung und Kontrolle darüber, was als Fehler protokolliert wird und wie es formatiert wird, was in größeren, komplexeren Anwendungen sehr hilfreich sein kann.
