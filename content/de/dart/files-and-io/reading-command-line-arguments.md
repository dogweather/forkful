---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:44.797705-07:00
description: "Das Lesen von Befehlszeilenargumenten in Dart erm\xF6glicht es Programmierern,\
  \ Daten direkt in die Konsole einzugeben, wenn ein Dart-Programm ausgef\xFChrt\u2026"
lastmod: '2024-03-11T00:14:27.492767-06:00'
model: gpt-4-0125-preview
summary: "Das Lesen von Befehlszeilenargumenten in Dart erm\xF6glicht es Programmierern,\
  \ Daten direkt in die Konsole einzugeben, wenn ein Dart-Programm ausgef\xFChrt\u2026"
title: Befehlszeilenargumente lesen
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten in Dart ermöglicht es Programmierern, Daten direkt in die Konsole einzugeben, wenn ein Dart-Programm ausgeführt wird, was dessen Interaktivität und Flexibilität für verschiedene Anwendungsfälle erhöht, einschließlich Automatisierungsskripte, CLI-Tools oder Stapelverarbeitung. Diese Funktion ist entscheidend für die Erstellung anpassbarer und benutzerfreundlicher Befehlszeilenanwendungen.

## Wie geht das:

Dart bietet einen unkomplizierten Zugang zu Befehlszeilenargumenten über die `List<String> args` in der Hauptmethode. Unten ist ein einfaches Beispiel, das zeigt, wie man Befehlszeilenargumente liest und nutzt.

```dart
// main.dart
void main(List<String> args) {
  print('Befehlszeilenargumente:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Um dieses Dart-Programm auszuführen und Befehlszeilenargumente zu übergeben, benutzen Sie die Dart CLI wie folgt:

```shell
dart run main.dart Hallo Welt!
```

Erwartete Ausgabe:

```
Befehlszeilenargumente:
1: Hallo
2: Welt!
```

### Nutzung einer beliebten Drittanbieterbibliothek: `args`

Obwohl die integrierten Fähigkeiten von Dart zum Umgang mit Befehlszeilenargumenten für viele Anwendungen robust sind, bietet das `args`-Paket eine verfeinerte Möglichkeit, Befehlszeilenargumente für komplexere Bedürfnisse zu definieren und zu parsen.

Fügen Sie zuerst das `args`-Paket zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  args: ^2.0.0
```

Dann verwenden Sie es in Ihrem Programm wie folgt:

```dart
// Nutzung des 'args'-Pakets
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hallo, ${argResults['name']}!');
  } else {
    print('Kein Name angegeben.');
  }
}
```

Führen Sie das Programm mit einem benannten Argument aus:

```shell
dart run main.dart --name=John
```

Erwartete Ausgabe:

```
Hallo, John!
```

Diese einfache Einführung in das Parsen von Befehlszeilenargumenten, sowohl nativ als auch mit der `args`-Bibliothek, zeigt, wie Dart Benutzereingaben direkt aus der Konsole verarbeiten kann, was den Weg für die Erstellung interaktiverer und dynamischerer CLI-Anwendungen eröffnet.
