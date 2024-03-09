---
title:                "Eine Textdatei lesen"
date:                  2024-03-08T21:55:38.265179-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei in Dart beinhaltet den Zugriff auf und das Abrufen von Daten aus auf dem Dateisystem gespeicherten Dateien. Programmierer tun dies, um Eingabedaten, Konfigurationseinstellungen oder Datensätze zu verarbeiten, was es zu einer grundlegenden Operation für viele Anwendungen macht – von einfachen Skripten bis hin zu komplexen Apps.

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
