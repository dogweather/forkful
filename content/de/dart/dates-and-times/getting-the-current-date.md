---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:40.310443-07:00
description: "Das Abrufen des aktuellen Datums in Dart beinhaltet eine Anfrage an\
  \ das System nach dem aktuellen Datum und der Uhrzeit. Diese Funktionalit\xE4t wird\
  \ h\xE4ufig\u2026"
lastmod: '2024-03-11T00:14:27.487049-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in Dart beinhaltet eine Anfrage an das\
  \ System nach dem aktuellen Datum und der Uhrzeit. Diese Funktionalit\xE4t wird\
  \ h\xE4ufig\u2026"
title: Das aktuelle Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Dart beinhaltet eine Anfrage an das System nach dem aktuellen Datum und der Uhrzeit. Diese Funktionalität wird häufig in Anwendungen für Funktionen wie Zeitstempel für Ereignisse, Anzeige des aktuellen Datums für Benutzer oder Berechnung von Dauern verwendet. Zu wissen, wie man das aktuelle Datum effizient abruft und manipuliert, ist grundlegend für Planung, Protokollierung und zeitabhängige Funktionen.

## Wie:
Die Kernbibliothek von Dart bietet direkten Zugriff auf das aktuelle Datum und die Uhrzeit über die `DateTime`-Klasse. Hier ist das grundlegende Beispiel, um das aktuelle Datum zu erhalten:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Beispiel-Ausgabe: 2023-04-12 10:00:00.000
}
```

Wenn Sie nur den Datumsanteil (Jahr, Monat, Tag) benötigen, können Sie das `DateTime`-Objekt formatieren:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Beispiel-Ausgabe: 2023-04-12
}
```

Dart enthält keine integrierte Bibliothek für komplexere Datumsformate, aber Sie können das `intl`-Paket für diesen Zweck verwenden. Fügen Sie zunächst das Paket zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  intl: ^0.17.0
```

Dann können Sie Daten einfach formatieren:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Beispiel-Ausgabe: 2023-04-12
}
```

Für fortgeschrittenere Formatierungsoptionen erkunden Sie die `DateFormat`-Klasse, die vom `intl`-Paket bereitgestellt wird, welche eine breite Palette von Mustern und Lokalisierungen unterstützt.
