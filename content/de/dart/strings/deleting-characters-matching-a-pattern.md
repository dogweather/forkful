---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:25.341233-07:00
description: "Wie: Dart macht es unkompliziert, Zeichen zu entfernen, die einem vordefinierten\
  \ Muster entsprechen, indem regul\xE4re Ausdr\xFCcke und die Methode `replaceAll`\u2026"
lastmod: '2024-03-13T22:44:53.562644-06:00'
model: gpt-4-0125-preview
summary: "Dart macht es unkompliziert, Zeichen zu entfernen, die einem vordefinierten\
  \ Muster entsprechen, indem regul\xE4re Ausdr\xFCcke und die Methode `replaceAll`\
  \ verwendet werden."
title: "Zeichen, die einem Muster entsprechen, l\xF6schen"
weight: 5
---

## Wie:
Dart macht es unkompliziert, Zeichen zu entfernen, die einem vordefinierten Muster entsprechen, indem reguläre Ausdrücke und die Methode `replaceAll` verwendet werden. Für die grundlegende Nutzung sind keine Drittanbieter-Bibliotheken erforderlich, was diesen Ansatz sehr zugänglich macht.

Hier ist ein einfaches Beispiel, das demonstriert, wie man Ziffern aus einem String entfernt:

```dart
void main() {
  String stringWithDigits = 'Dart123 macht Spaß456';
  // Definiere ein reguläres Ausdrucksmuster, das alle Ziffern entspricht
  RegExp digitPattern = RegExp(r'\d');
  
  // Ersetze alle Vorkommen des Musters mit einem leeren String
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // Ausgabe: Dart macht Spaß
}
```

Angenommen, Sie haben es mit einem komplexeren Szenario zu tun, wie z.B. dem Entfernen von Sonderzeichen, außer Leerzeichen und Satzzeichen. So würden Sie es machen:

```dart
void main() {
  String messyString = 'Dart!@# macht *&()Spaß$%^';
  // Definiere ein Muster, das alles außer Buchstaben, Zahlen, Leerzeichen und Satzzeichen entspricht
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // Ausgabe: Dart! macht Spaß
}
```

Für Aufgaben, die eine fortgeschrittenere Musterabgleichung und -ersetzung erfordern, bietet die umfassende `RegExp`-Klassendokumentation von Dart einen tiefen Einblick in komplexere Ausdrücke und deren Verwendung. Die oben genannten Beispiele decken jedoch die Mehrheit der gängigen Anwendungsfälle ab, um Zeichen basierend auf Mustern in der Dart-Programmierung zu löschen.
