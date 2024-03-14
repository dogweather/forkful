---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:00.581557-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) in Dart bieten eine leistungsf\xE4\
  hige M\xF6glichkeit, Strings zu suchen und zu manipulieren, wodurch Programmierer\
  \ komplexe\u2026"
lastmod: '2024-03-13T22:44:53.569238-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) in Dart bieten eine leistungsf\xE4hige\
  \ M\xF6glichkeit, Strings zu suchen und zu manipulieren, wodurch Programmierer komplexe\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (regex) in Dart bieten eine leistungsfähige Möglichkeit, Strings zu suchen und zu manipulieren, wodurch Programmierer komplexe Textverarbeitungsaufgaben effizient durchführen können. Durch das Verständnis von regex können Entwickler Textvalidierungen, Suchmuster und Texttransformationen schnell ausführen, was für die Verarbeitung von Formularen, Daten-Parsing und allgemeine String-Manipulationen in modernen Anwendungen wesentlich ist.

## Wie:
Dart verwendet die `RegExp`-Klasse für reguläre Ausdrücke. Hier ist ein einfaches Beispiel, um ein einfaches Muster innerhalb eines Strings zu finden:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Das Erlernen der Dart-Programmierung ist aufregend.';

  if (pattern.hasMatch(text)) {
    print('Übereinstimmung gefunden!');
  } else {
    print('Keine Übereinstimmung gefunden.');
  }
  // Ausgabe: Übereinstimmung gefunden!
}
```

Um Übereinstimmungen aus einem String zu extrahieren, können Sie die Methode `allMatches` verwenden. Diese Methode gibt ein iterierbares Objekt von Übereinstimmungen zurück:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart ist großartig!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Das druckt die gefundenen Teilstrings.
  }
  // Ausgabe:
  // Dart
  // ist
  // großartig
}
```

Text ersetzen kann erreicht werden, indem man die Methoden `replaceFirst` oder `replaceAll` verwendet:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart ist nicht nur ein Dart.';
  
  // Erstes Vorkommen ersetzen
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Ausgabe: Flutter ist nicht nur ein Dart.

  // Alle Vorkommen ersetzen
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Ausgabe: Flutter ist nicht nur ein Flutter.
}
```

Einen String durch ein regex Muster zu teilen, ist einfach mit der Methode `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Passt auf jedes Leerzeichen
  var text = 'Dart macht Spaß';

  var parts = text.split(pattern);
  print(parts); 
  // Ausgabe: [Dart, macht, Spaß]
}
```

Für komplexe Parsing- oder Validierungsaufgaben, die nicht direkt von Darts `RegExp` unterstützt werden, könnten Sie überlegen, Bibliotheken von Drittanbietern zu verwenden, aber die Standardbibliothek von Dart ist oft ausreichend für gängige regex-Aufgaben, was ihren Nutzen und ihre Vielseitigkeit bei der Handhabung von regulären Ausdrücken unterstreicht.
