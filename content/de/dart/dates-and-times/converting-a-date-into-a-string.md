---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:26.628435-07:00
description: "Wie: Dart bietet die `DateTime`-Klasse zur Handhabung von Daten und\
  \ Zeiten und das `intl`-Paket f\xFCr das Formatieren. Stellen Sie zun\xE4chst sicher,\
  \ dass Sie\u2026"
lastmod: '2024-03-13T22:44:53.593499-06:00'
model: gpt-4-0125-preview
summary: "Dart bietet die `DateTime`-Klasse zur Handhabung von Daten und Zeiten und\
  \ das `intl`-Paket f\xFCr das Formatieren."
title: Ein Datum in einen String umwandeln
weight: 28
---

## Wie:
Dart bietet die `DateTime`-Klasse zur Handhabung von Daten und Zeiten und das `intl`-Paket für das Formatieren. Stellen Sie zunächst sicher, dass Sie das `intl`-Paket haben, indem Sie `intl: ^0.17.0` (oder die neueste Version) zu Ihrer `pubspec.yaml`-Datei hinzufügen.

### Verwendung der Dart-Kernbibliothek
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Ausgabe: 2023-4-12 (zum Beispiel, dies hängt vom aktuellen Datum ab)
```

Dieses Beispiel konstruiert direkt einen String aus den Eigenschaften von `DateTime`.

### Verwendung des `intl`-Pakets
Zuerst importieren Sie das Paket:

```dart
import 'package:intl/intl.dart';
```

Dann formatieren Sie das Datum:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Ausgabe: 2023-04-12
```

Das `intl`-Paket ermöglicht viel komplexeres Formatieren einfach, einschließlich lokal-spezifischer Formate:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Ausgabe: April 12, 2023
```

Diese Beispiele zeigen einfache, aber leistungsstarke Wege, um Daten in Dart in Strings zu konvertieren und zu formatieren, entweder unter Verwendung der Kernfunktionalität von Dart oder durch Nutzung des `intl`-Pakets für fortgeschrittenere Formatierungsoptionen.
