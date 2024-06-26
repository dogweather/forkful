---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:38.265179-07:00
description: "Wie geht das: Darts Kernbibliothek, `dart:io`, bietet die notwendigen\
  \ Funktionen, um Textdateien synchron oder asynchron zu lesen. Hier ist, wie man\u2026"
lastmod: '2024-03-13T22:44:53.600025-06:00'
model: gpt-4-0125-preview
summary: Darts Kernbibliothek, `dart:io`, bietet die notwendigen Funktionen, um Textdateien
  synchron oder asynchron zu lesen.
title: Eine Textdatei lesen
weight: 22
---

## Wie geht das:
Darts Kernbibliothek, `dart:io`, bietet die notwendigen Funktionen, um Textdateien synchron oder asynchron zu lesen. Hier ist, wie man beides angeht.

**Synchron:**

```dart
import 'dart:io';

void main() {
  var fileName = "Pfad/zu/deiner/Textdatei.txt";
  var file = File(fileName);

  // Die Datei synchron lesen
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Fehler beim Lesen der Datei: $e');
  }
}
```

**Asynchron:**

Um zu vermeiden, dass das Programm blockiert wird, während die Datei gelesen wird, was besonders für große Dateien oder reaktionsfähige Anwendungen nützlich ist:

```dart
import 'dart:io';

void main() async {
  var fileName = "Pfad/zu/deiner/Textdatei.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Fehler beim Lesen der Datei: $e');
  }
}
```

**Beispielausgabe:**

Wenn deine Textdatei den Inhalt hat:

```
Hallo, Dart!
```

Beide oben genannten Methoden werden ausgeben:

```
Hallo, Dart!
```

**Verwendung einer Drittanbieter-Bibliothek:**

Für zusätzliche Funktionen, wie vereinfachte Dateioperationen oder verbesserte Fehlerbehandlung, könnten Sie Drittanbieter-Bibliotheken wie `package:file` in Betracht ziehen. Jedoch, wie bei meinem letzten Update, ist die direkte Verwendung des Kernpakets `dart:io`, wie oben gezeigt, die gängigste und unkomplizierteste Methode zum Lesen von Textdateien in Dart.
