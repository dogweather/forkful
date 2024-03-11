---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:49.779768-07:00
description: "Assoziative Arrays in Dart, allgemein bekannt als Maps, sind Datenstrukturen,\
  \ die Daten in Schl\xFCssel-Wert-Paaren speichern. Sie erm\xF6glichen es\u2026"
lastmod: '2024-03-11T00:14:27.466306-06:00'
model: gpt-4-0125-preview
summary: "Assoziative Arrays in Dart, allgemein bekannt als Maps, sind Datenstrukturen,\
  \ die Daten in Schl\xFCssel-Wert-Paaren speichern. Sie erm\xF6glichen es\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays in Dart, allgemein bekannt als Maps, sind Datenstrukturen, die Daten in Schlüssel-Wert-Paaren speichern. Sie ermöglichen es Programmierern, auf Elemente nicht über Indizes, sondern über Schlüssel zuzugreifen, was das Abrufen von Daten intuitiv und effizient macht, insbesondere beim Arbeiten mit strukturierten Daten, bei denen jedes Element einen eindeutigen Identifikator hat.

## Wie geht das:

Dart bietet eine unkomplizierte Syntax zum Erstellen und Manipulieren von Maps. Unten finden Sie Beispiele, die grundlegende Operationen wie Erstellung, Hinzufügen von Elementen und Abrufen von Werten demonstrieren.

```dart
void main() {
  // Eine Map erstellen
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // Ein neues Schlüssel-Wert-Paar hinzufügen
  fruitColors['orange'] = 'orange';

  // Auf einen Wert über seinen Schlüssel zugreifen
  print(fruitColors['apple']); // Ausgabe: red

  // Einen Wert aktualisieren
  fruitColors['banana'] = 'green';

  // Über die Map iterieren
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // Beispiel-Ausgabe:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

Für komplexe Datenstrukturen oder erweiterte Funktionalitäten verlassen sich Dart-Programmierer oft auf zusätzliche Bibliotheken. Eine solche Bibliothek ist `collection`, die erweiterte Sammlungstypen und Hilfsfunktionen bietet. Obwohl `collection` die grundlegende Handhabung von Maps nicht verändert, bereichert sie diese mit Hilfsfunktionen und ausgefeilteren Sammlungstypen. So könnten Sie sie für eine spezifischere Aufgabe verwenden, wie z.B. das Sortieren einer Map nach ihren Werten:

Stellen Sie zunächst sicher, dass das `collection`-Paket in Ihrer `pubspec.yaml`-Datei enthalten ist:

```yaml
dependencies:
  collection: ^1.15.0
```

Dann können Sie es wie folgt verwenden:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // Die Map nach ihren Werten (Farben) sortieren
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // Ausgabe:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

Dieses Beispiel demonstriert das Sortieren der Einträge einer Map basierend auf ihren Werten und zeigt, wie Dart und sein lebendiges Ökosystem geschickt mit assoziativen Arrays für eine ausgefeiltere Datenmanipulation umgehen können.
