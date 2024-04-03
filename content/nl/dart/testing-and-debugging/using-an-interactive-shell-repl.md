---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:53.866147-07:00
description: "Hoe te: Dart wordt niet geleverd met een ingebouwde REPL. Echter, je\
  \ kunt REPL-achtige functionaliteit bereiken door gebruik te maken van DartPad (online)\u2026"
lastmod: '2024-03-13T22:44:50.506587-06:00'
model: gpt-4-0125-preview
summary: Dart wordt niet geleverd met een ingebouwde REPL.
title: Het Gebruik van een Interactieve Shell (REPL)
weight: 34
---

## Hoe te:
Dart wordt niet geleverd met een ingebouwde REPL. Echter, je kunt REPL-achtige functionaliteit bereiken door gebruik te maken van DartPad (online) of door derde partij tools zoals `dart_repl` te gebruiken.

**DartPad gebruiken:**

DartPad (https://dartpad.dev) is een online Dart editor waarmee je Dart code in je webbrowser kunt schrijven en uitvoeren. Hoewel het geen traditionele command-line REPL is, biedt het een vergelijkbare ervaring voor snelle experimenten.

Ga simpelweg naar de website, typ je Dart code in het linkerpaneel en klik op "Uitvoeren" om de output aan de rechterkant te zien.

Voorbeeld:
```dart
void main() {
  print('Hallo, Dart!');
}
```
Output:
```
Hallo, Dart!
```

**`dart_repl` gebruiken (tool van derde partij):**

Installeer eerst `dart_repl` via pub globaal:

```shell
dart pub global activate dart_repl
```

Voer vervolgens `dart_repl` uit vanuit je terminal:

```shell
dart_repl
```

Nu kun je direct Dart uitspraken typen in de shell. Bijvoorbeeld:

```dart
>>> print('Hallo, REPL!');
Hallo, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Deze methoden bieden een snelle weg om Dart code ter plekke uit te proberen, wat de leercurve aanzienlijk verzacht en de productiviteit verhoogt.
