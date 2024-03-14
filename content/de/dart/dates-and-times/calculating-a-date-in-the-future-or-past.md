---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:42.681926-07:00
description: "Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine\
  \ g\xE4ngige Aufgabe f\xFCr Programmierer, die sich mit Terminplanung, Erinnerungen\
  \ oder\u2026"
lastmod: '2024-03-13T22:44:53.595763-06:00'
model: gpt-4-0125-preview
summary: "Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine g\xE4\
  ngige Aufgabe f\xFCr Programmierer, die sich mit Terminplanung, Erinnerungen oder\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist eine gängige Aufgabe für Programmierer, die sich mit Terminplanung, Erinnerungen oder jeder Funktion befassen, die von Datumsberechnungen abhängt. Das Verständnis, wie man mit Daten umgeht, ist entscheidend für Backend-Systeme, Benutzeroberflächen und Datenanalysen, besonders für diejenigen, die zu Dart wechseln und die zeitliche Logik effizient implementieren möchten.

## Wie:
Dart bietet robuste Unterstützung für die Datumsmanipulation durch seine `DateTime` Klasse. So können Sie zukünftige oder vergangene Daten mit nativem Dart berechnen, ohne dass Sie Drittanbieterbibliotheken benötigen.

### Berechnung eines zukünftigen Datums
Um ein Datum in der Zukunft zu berechnen, erstellen Sie ein `DateTime` Objekt und verwenden die Methode `add` mit der gewünschten Dauer.

```dart
DateTime heute = DateTime.now();
Duration zehnTage = Duration(days: 10);
DateTime zukuenftigesDatum = heute.add(zehnTage);

print(zukuenftigesDatum); // Ausgabe: 2023-04-21 14:22:35.123456 (Beispielausgabe, hängt vom aktuellen Datum und Zeit ab)
```

### Berechnung eines vergangenen Datums
Um ein Datum in der Vergangenheit zu berechnen, verwenden Sie die Methode `subtract` an einem `DateTime` Objekt mit der notwendigen Dauer.

```dart
DateTime heute = DateTime.now();
Duration fuenfzehnTageZurueck = Duration(days: 15);
DateTime vergangenesDatum = heute.subtract(fuenfzehnTageZurueck);

print(vergangenesDatum); // Ausgabe: 2023-03-27 14:22:35.123456 (Beispielausgabe, hängt vom aktuellen Datum und Zeit ab)
```

### Verwendung von Drittanbieterbibliotheken
Obwohl die nativen Fähigkeiten von Dart zur Datumsmanipulation leistungsfähig sind, könnten Sie sich in Situationen befinden, in denen Sie spezifischere Operationen benötigen, wie z.B. das einfache Parsen oder Formatieren von Daten oder das Durchführen komplexer Berechnungen. In solchen Fällen kann das `time` Paket sehr nützlich sein.

Zuerst fügen Sie `time` zu Ihren `pubspec.yaml` Abhängigkeiten hinzu:

```yaml
dependencies:
  time: ^2.0.0
```

Danach können Sie es verwenden, um ähnliche Berechnungen mit verbesserter Lesbarkeit durchzuführen:

```dart
import 'package:time/time.dart';

void main() {
  DateTime heute = DateTime.now();

  // Berechnung eines zukünftigen Datums
  DateTime zukuenftigesDatum = heute + 10.days;
  print(zukuenftigesDatum); // Ausgabeformat: 2023-04-21 14:22:35.123456

  // Berechnung eines vergangenen Datums
  DateTime vergangenesDatum = heute - 15.days;
  print(vergangenesDatum); // Ausgabeformat: 2023-03-27 14:22:35.123456
}
```

Diese Beispiele veranschaulichen grundlegende Datumsmanipulationen in Dart, einschließlich des Hinzufügens und Subtrahierens von Zeit zu oder von einem aktuellen Datum, und demonstrieren, wie mühelos Daten in Dart-Anwendungen verwaltet werden können.
