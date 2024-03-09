---
title:                "Zahlen runden"
date:                  2024-03-08T21:56:38.163569-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Runden von Zahlen ist der Vorgang, eine Zahl auf die nächstgelegene ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen anzupassen. Programmierer runden Zahlen oft, um Berechnungen zu vereinfachen, die Lesbarkeit zu verbessern oder Daten für die Anzeige vorzubereiten, was Konsistenz und Klarheit in den numerischen Ausgaben sicherstellt.

## Wie geht das:

Dart stellt native Methoden in seinem Kern `num` Typ für Rundungsoperationen zur Verfügung. Hier werden wir Methoden wie `round()`, `floor()`, `ceil()` und das Runden auf eine spezifische Anzahl von Dezimalstellen erkunden.

### Auf die nächste ganze Zahl runden:

```dart
var number = 3.56;
print(number.round()); // Gibt aus: 4
```

### Abrunden:

```dart
print(number.floor()); // Gibt aus: 3
```

### Aufrunden:

```dart
print(number.ceil()); // Gibt aus: 4
```

### Auf eine spezifische Anzahl von Dezimalstellen runden:

Um auf eine spezifische Anzahl von Dezimalstellen zu runden, können wir die Methode `toStringAsFixed()` verwenden, die einen String zurückgibt, oder eine Kombination aus `pow` von `dart:math` für ein numerisches Ergebnis nutzen.

```dart
import 'dart:math';

var number = 3.56789;
String gerundeterString = number.toStringAsFixed(2); // Zu Anzeigezwecken
print(gerundeterString); // Gibt aus: 3.57

double gerundeteZahl = double.parse(gerundeterString);
print(gerundeteZahl); // Gibt aus: 3.57

// Alternativ, für ein numerisches Ergebnis:
double aufDezimalGerundet = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(aufDezimalGerundet); // Gibt aus: 3.57
```

Während die Kernbibliothek von Dart die meisten Rundungsbedürfnisse effektiv abdeckt, können für komplexere mathematische Operationen oder genaue Rundungsanforderungen Bibliotheken wie `decimal` nützlich sein. Die `decimal` Bibliothek bietet eine einfache Möglichkeit, mit Dezimalzahlen ohne Präzisionsverlust zu arbeiten, was besonders praktisch für Finanzberechnungen ist, aber für einfache Rundungsmethoden wie gezeigt, ist die Kernfunktionalität von Dart in der Regel ausreichend.
