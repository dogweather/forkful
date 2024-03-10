---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.776917-07:00
description: "Das Suchen und Ersetzen von Text in Dart beinhaltet die Untersuchung\
  \ von Zeichenketten, um bestimmte Muster oder Zeichenfolgen zu finden und diese\
  \ durch\u2026"
lastmod: '2024-03-09T21:06:17.560789-07:00'
model: gpt-4-0125-preview
summary: "Das Suchen und Ersetzen von Text in Dart beinhaltet die Untersuchung von\
  \ Zeichenketten, um bestimmte Muster oder Zeichenfolgen zu finden und diese durch\u2026"
title: Suchen und Ersetzen von Text
---

{{< edit_this_page >}}

## Was & Warum?

Das Suchen und Ersetzen von Text in Dart beinhaltet die Untersuchung von Zeichenketten, um bestimmte Muster oder Zeichenfolgen zu finden und diese durch neue Inhalte zu ersetzen. Diese Operation ist grundlegend für Aufgaben wie Datenvalidierung, Formatierung von Ausgaben, Parsen von Benutzereingaben oder sogar die Manipulation von URLs und Dateipfaden, wodurch Anwendungen dynamischer und reaktiver auf die Bedürfnisse der Benutzer gemacht werden.

## Wie:

Dart bietet robuste Methoden zum Suchen und Ersetzen von Text direkt über seine `String`-Klasse, ohne die Notwendigkeit externer Bibliotheken. Hier ist, wie Sie es machen können:

### Grundlegendes Suchen und Ersetzen

Um nach einer Teilzeichenkette zu suchen und sie durch eine andere Zeichenkette zu ersetzen, können Sie `replaceAll` verwenden:

```dart
String beispieltext = "Hallo, Dart! Dart ist großartig.";
String geaenderterText = beispieltext.replaceAll("Dart", "Flutter");
print(geaenderterText); // Ausgabe: Hallo, Flutter! Flutter ist großartig.
```

### Verwendung von Regulären Ausdrücken

Für komplexere Such- und Ersetzungsbedürfnisse nutzt Dart reguläre Ausdrücke über die `RegExp`-Klasse. Dies ermöglicht die Mustererkennung und den Ersatz in Zeichenketten:

```dart
String beispieltext = "Dart 2023, Flutter 2023";
String geaenderterText = beispieltext.replaceAll(RegExp(r'\d+'), "2024");
print(geaenderterText); // Ausgabe: Dart 2024, Flutter 2024
```

In diesem Beispiel werden alle Instanzen von einer oder mehreren Ziffern (`\d+`) im String gefunden und durch "2024" ersetzt.

### Groß- und Kleinschreibung Unberücksichtigt Lassen

Um eine Groß- und Kleinschreibung ignorierende Suche durchzuführen, können Sie den `RegExp`-Konstruktor so modifizieren, dass die Groß-/Kleinschreibung ignoriert wird:

```dart
String beispieltext = "Willkommen bei Dart, der Programmiersprache.";
String geaenderterText = beispieltext.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(geaenderterText); // Ausgabe: Willkommen bei Flutter, der Programmiersprache.
```

### Ersetzen mit einer Funktion

Für dynamische Ersetzungen, basierend auf dem eigentlichen Treffer, erlaubt Dart die Übergabe einer Funktion an `replaceAllMapped`. Diese Funktion kann Operationen oder Berechnungen auf den gefundenen Sequenzen ausführen:

```dart
String beispieltext = "Erhöhe 5 um 1, um 6 zu erhalten.";
String erhoehterText = beispieltext.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(erhoehterText); // Ausgabe: Erhöhe 6 um 1, um 7 zu erhalten.
```

Hierbei wird jede Ziffernsequenz durch ihren Inkrementwert ersetzt. Jeder Treffer wird in einen Integer umgewandelt, erhöht und dann zurück in einen String für den Ersatz konvertiert.

Die Fähigkeiten von Dart zur Zeichenkettenmanipulation, insbesondere zum Suchen und Ersetzen von Text, machen es zu einem leistungsstarken Werkzeug für die Verarbeitung und Vorbereitung von Daten in Ihren Anwendungen. Ob durch einfache Zeichenkettenersetzungen oder durch die Nutzung der Kraft regulärer Ausdrücke, Dart bietet die Flexibilität und Leistung, die für effektive Textmanipulation benötigt wird.
