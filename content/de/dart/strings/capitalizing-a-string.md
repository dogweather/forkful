---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:22.367911-07:00
description: "Das Gro\xDFschreiben eines Strings beinhaltet das Modifizieren des ersten\
  \ Buchstabens eines Wortes oder eines ganzen Satzes in Gro\xDFbuchstaben, w\xE4\
  hrend der\u2026"
lastmod: '2024-03-13T22:44:53.561517-06:00'
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings beinhaltet das Modifizieren des ersten\
  \ Buchstabens eines Wortes oder eines ganzen Satzes in Gro\xDFbuchstaben, w\xE4\
  hrend der\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings beinhaltet das Modifizieren des ersten Buchstabens eines Wortes oder eines ganzen Satzes in Großbuchstaben, während der Rest der Zeichen unverändert bleibt. Programmierer verwenden diese Technik häufig beim Formatieren von Benutzereingaben oder der Textanzeige, um Konsistenz sicherzustellen oder grammatikalischen Regeln in Benutzeroberflächen zu folgen.

## Wie geht das:

### Mit Dart's eingebauten Methoden

Dart stellt einfache, unkomplizierte Methoden für die String-Manipulation bereit. Um ein Wort oder einen Satz zu groß zu schreiben, würden Sie typischerweise den ersten Charakter nehmen, in einen Großbuchstaben umwandeln und dann mit dem Rest des Strings zusammenfügen. So könnten Sie es implementieren:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Ausgabe: Hello world
}
```

### Jedes Wort großschreiben

Um den ersten Buchstaben jedes Wortes in einem String großzuschreiben, könnten Sie den String in Wörter teilen, jedes einzelne großschreiben und sie dann wieder zusammenfügen:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Ausgabe: Hello Dart Enthusiasts
}
```

### Verwendung von Drittanbieter-Bibliotheken

Während Dart's Standardbibliothek grundlegende Bedürfnisse abdeckt, könnten bestimmte Aufgaben mit Drittanbieter-Paketen bequemer erledigt werden. Eine beliebte Wahl für erweiterte String-Manipulationsfähigkeiten, einschließlich Großschreibung, ist das [`recase`](https://pub.dev/packages/recase) Paket. Nachdem Sie es zu Ihrem Projekt in `pubspec.yaml` hinzugefügt haben, können Sie einfach Strings großschreiben unter anderem Funktionalitäten:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Ausgabe: Hello World
}
```

Mit `recase` können Sie einzelne Wörter, ganze Sätze großschreiben oder sogar anderen Schreibkonventionen folgen, ohne die String-Transformationen manuell vorzunehmen.
