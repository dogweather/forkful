---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.849102-07:00
description: "Das Ausgeben von Debug-Informationen in Dart bezieht sich darauf, Informationen\
  \ w\xE4hrend der Laufzeit in der Konsole anzuzeigen, was Entwicklern\u2026"
lastmod: '2024-03-11T00:14:27.477805-06:00'
model: gpt-4-0125-preview
summary: "Das Ausgeben von Debug-Informationen in Dart bezieht sich darauf, Informationen\
  \ w\xE4hrend der Laufzeit in der Konsole anzuzeigen, was Entwicklern\u2026"
title: Debug-Ausgabe drucken
---

{{< edit_this_page >}}

## Was & Warum?

Das Ausgeben von Debug-Informationen in Dart bezieht sich darauf, Informationen während der Laufzeit in der Konsole anzuzeigen, was Entwicklern ermöglicht, den Ablauf der Ausführung zu verfolgen, den Zustand von Variablen zu untersuchen oder die Quelle von Fehlern zu identifizieren. Programmierer nutzen dies häufig zur Fehlersuche und um zu überprüfen, ob ihr Code sich wie erwartet verhält, was einen reibungsloseren und effizienteren Entwicklungsprozess fördert.

## Wie:

In Dart können Sie Debug-Ausgaben mit der `print()`-Funktion ausgeben. Hier erfahren Sie, wie Sie einfache Nachrichten und Variablenwerte ausgeben:

```dart
void main() {
  String greeting = "Hallo, Dart!";
  print(greeting); // Gibt aus: Hallo, Dart!

  int number = 42;
  print('Die Zahl ist $number.'); // Gibt aus: Die Zahl ist 42.
}
```

Für strukturierte Daten, wie Listen oder Objekte, bietet die `toString()`-Methode von Dart möglicherweise nicht genug Details. In diesen Fällen können Sie die Funktion `jsonEncode` aus der Dart-Bibliothek `dart:convert` verwenden, um die Daten in einen JSON-String zu konvertieren und so eine lesbare Ausgabe zu erhalten:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // Gibt aus: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

Wenn anspruchsvollere Debugging-Funktionalitäten benötigt werden, wie das Protokollieren mit unterschiedlichen Wichtigkeitsgraden (Info, Warnung, Fehler), können Sie Drittanbieter-Bibliotheken wie `logger` verwenden. So verwenden Sie sie:

1. Fügen Sie `logger` zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Verwenden Sie `logger` in Ihrem Dart-Code:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Dies ist eine Debug-Nachricht");
  logger.w("Dies ist eine Warnmeldung");
  logger.e("Dies ist eine Fehlermeldung");
}
```

Die Ausgabe wird informativer sein, sie zeigt das Niveau der Nachricht und die Nachricht selbst, was es einfacher macht, zwischen verschiedenen Arten von Protokollnachrichten zu unterscheiden.
