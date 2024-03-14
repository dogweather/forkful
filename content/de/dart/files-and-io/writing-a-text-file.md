---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:42.594043-07:00
description: "Das Schreiben einer Textdatei in Dart beinhaltet das Erstellen oder\
  \ Modifizieren von Dateien auf der Festplatte, um Daten in einem lesbaren Format\
  \ zu\u2026"
lastmod: '2024-03-13T22:44:53.601053-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben einer Textdatei in Dart beinhaltet das Erstellen oder Modifizieren\
  \ von Dateien auf der Festplatte, um Daten in einem lesbaren Format zu\u2026"
title: Eine Textdatei schreiben
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei in Dart beinhaltet das Erstellen oder Modifizieren von Dateien auf der Festplatte, um Daten in einem lesbaren Format zu speichern. Programmierer tun dies, um Anwendungsdaten, Konfigurationen, Protokolle oder jegliche Informationen, die zwischen den Ausführungen der Anwendung bestehen bleiben sollten oder Daten mit anderen Anwendungen oder Benutzern teilen sollen, zu speichern.

## Wie:
Die Kernbibliothek von Dart bietet das `dart:io` Paket für die Dateibehandlung, das es Ihnen ermöglicht, ohne die Notwendigkeit von Drittanbieterbibliotheken, Textdateien zu schreiben. Hier ist ein einfaches Beispiel für das Schreiben einer Textdatei:

```dart
import 'dart:io';

void main() async {
  // Erstelle eine neue Datei namens 'example.txt' im aktuellen Verzeichnis.
  var file = File('example.txt');
  
  // Schreibe einen String in die Datei.
  await file.writeAsString('Hallo, Dart!');
  
  // Überprüfe den Inhalt.
  print(await file.readAsString()); // Ausgabe: Hallo, Dart!
}
```

Wenn Sie mit größeren Dateien oder Datenströmen arbeiten, bevorzugen Sie möglicherweise das Schreiben von Inhalten mit `openWrite`, das ein `IOSink` zurückgibt und es Ihnen ermöglicht, Daten in Blöcken zu schreiben:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Schreibe mehrere Zeilen in die Datei.
  sink
    ..writeln('Zeile 1: Der schnelle braune Fuchs springt über den faulen Hund.')
    ..writeln('Zeile 2: Dart ist fantastisch!')
    ..close();

  // Warte, bis der Sink geschlossen ist, um sicherzustellen, dass alle Daten in die Datei geschrieben sind.
  await sink.done;

  // Lese und drucke den Dateiinhalt zur Überprüfung
  print(await file.readAsString());
}
```

Für fortgeschrittenere Dateioperationen, einschließlich des Anhängens an Dateien oder des Schreibens von Bytes, könnten Sie tiefer in die Methoden der `File` Klasse, die von `dart:io` bereitgestellt werden, eindringen. Zusätzlich, beim Arbeiten an groß angelegten oder komplexeren Projekten, könnte die Berücksichtigung von Paketen wie `path` für die Dateipfadbehandlung oder `shelf` für Webserverfunktionalitäten nützlich sein, obwohl das direkte Schreiben von Dateien typischerweise auf den integrierten Dart-Bibliotheken basiert.
