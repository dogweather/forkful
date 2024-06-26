---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.950998-07:00
description: "Wie: In Dart k\xF6nnen Sie verschiedene Methoden verwenden, um Teilzeichenketten\
  \ zu extrahieren, wie `substring()`, `split()` und regul\xE4re Ausdr\xFCcke. Jede\u2026"
lastmod: '2024-03-13T22:44:53.568195-06:00'
model: gpt-4-0125-preview
summary: "In Dart k\xF6nnen Sie verschiedene Methoden verwenden, um Teilzeichenketten\
  \ zu extrahieren, wie `substring()`, `split()` und regul\xE4re Ausdr\xFCcke."
title: Teilstrings extrahieren
weight: 6
---

## Wie:
In Dart können Sie verschiedene Methoden verwenden, um Teilzeichenketten zu extrahieren, wie `substring()`, `split()` und reguläre Ausdrücke. Jede Methode dient unterschiedlichen Zwecken und bietet Flexibilität im Umgang mit Zeichenketten.

### Verwendung von `substring()`:
Die Methode `substring()` ist unkompliziert. Sie geben den Start- (und optional den End-) Index an, um die Zeichenkette zu teilen.

```dart
void main() {
  String example = "Hallo, Welt!";
  String result = example.substring(7, 12);
  print(result); // Ausgabe: Welt
}
```

### Verwendung von `split()`:
Teilen Sie eine Zeichenkette in eine Liste von Teilzeichenketten basierend auf einem Muster (wie einem Leerzeichen oder Komma) und greifen Sie dann über den Index auf die Teilzeichenkette zu.

```dart
void main() {
  String example = "Dart macht Spaß";
  List<String> parts = example.split(' ');
  String result = parts[1]; // Zugriff über Index
  print(result); // Ausgabe: macht
}
```

### Verwendung von Regulären Ausdrücken:
Für komplexe Muster ist die `RegExp` Klasse von Dart leistungsfähig. Verwenden Sie sie, um Muster zu finden und Teilzeichenketten zu extrahieren.

```dart
void main() {
  String example = "Email: beispiel@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // Ausgabe: beispiel@mail.com
}
```

### Drittanbieter-Bibliotheken:
Obwohl die Standardbibliothek von Dart ziemlich leistungsfähig ist, könnten Sie auf Szenarien stoßen, in denen eine Bibliothek eines Drittanbieters Ihre Aufgabe vereinfachen könnte. Eine beliebte Wahl für die Zeichenkettenmanipulation und Mustererkennung wird hier speziell nicht befürwortet, da die integrierten Funktionen von Dart oft ausreichen. Überprüfen Sie jedoch immer [pub.dev](https://pub.dev) for any libraries that might suit your specific needs better.
