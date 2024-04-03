---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.511163-07:00
description: "Die L\xE4nge eines Strings in Dart zu finden, bedeutet, die Anzahl der\
  \ Codeeinheiten (im Grunde die Anzahl der Zeichen, wenn man es vereinfacht betrachtet)\u2026"
lastmod: '2024-03-13T22:44:53.570581-06:00'
model: gpt-4-0125-preview
summary: "Die L\xE4nge eines Strings in Dart zu finden, bedeutet, die Anzahl der Codeeinheiten\
  \ (im Grunde die Anzahl der Zeichen, wenn man es vereinfacht betrachtet) in einem\
  \ gegebenen String zu bestimmen."
title: "Die L\xE4nge einer Zeichenkette finden"
weight: 7
---

## Wie:
Dart macht es unkompliziert, die Länge eines Strings mit der `length` Eigenschaft zu erhalten. Hier ist ein einfaches Beispiel:

```dart
void main() {
  String myString = "Hallo, Dart!";
  print("Die Länge von '\(myString)' ist: \(myString.length)");
  // Ausgabe: Die Länge von 'Hallo, Dart!' ist: 12
}
```
Diese Eigenschaft zählt die Anzahl der UTF-16-Codeeinheiten in dem String, was für die meisten gängigen Anwendungsfälle der Länge des Strings entspricht.

Für nuanciertere Textverarbeitungen, besonders bei Unicode-Zeichen außerhalb der Basic Multilingual Plane (BMP), sollte man das `characters` Paket für die Zählung von Graphemclustern in Betracht ziehen, das die vom Benutzer wahrgenommenen Zeichen genauer darstellt.

Fügen Sie zunächst `characters` zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  characters: ^1.2.0
```

Verwenden Sie es dann wie folgt:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 Familie";
  print("Die Länge von '\(myEmojiString)' ist: \(myEmojiString.characters.length)");
  // Ausgabe: Die Länge von '👨‍👩‍👧‍👦 Familie' ist: 8
}
```

In diesem Beispiel gibt uns `myEmojiString.characters.length` die Länge in Begriffen von Unicode-Graphemclustern an, was für Strings, die komplexe Zeichen enthalten, wie Emojis oder zusammengesetzte Zeichenmarkierungen, eine genauere Darstellung ist.
