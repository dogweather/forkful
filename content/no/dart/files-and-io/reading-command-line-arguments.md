---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:41.696944-07:00
description: "Hvordan: Dart tilbyr en enkel tilgang til kommandolinjeargumenter via\
  \ `List<String> args` i hovedmetoden. Nedenfor er et enkelt eksempel som demonstrerer\u2026"
lastmod: '2024-03-13T22:44:40.504616-06:00'
model: gpt-4-0125-preview
summary: Dart tilbyr en enkel tilgang til kommandolinjeargumenter via `List<String>
  args` i hovedmetoden.
title: Lese kommandolinje-argumenter
weight: 23
---

## Hvordan:
Dart tilbyr en enkel tilgang til kommandolinjeargumenter via `List<String> args` i hovedmetoden. Nedenfor er et enkelt eksempel som demonstrerer hvordan man leser og utnytter kommandolinjeargumenter.

```dart
// main.dart
void main(List<String> args) {
  print('Kommandolinjeargumenter:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

For å kjøre dette Dart-programmet og sende kommandolinjeargumenter, bruk Dart CLI slik:

```shell
dart run main.dart Hallo Verden!
```

Forventet output:

```
Kommandolinjeargumenter:
1: Hallo
2: Verden!
```

### Bruk av et Populært Tredjepartsbibliotek: `args`
Selv om Darts innebygde kapasiteter for håndtering av kommandolinjeargumenter er robuste for mange applikasjoner, tilbyr `args`-pakken en forfinet måte å definere og analysere kommandolinjeargumenter på for mer komplekse behov.

Først, legg til `args`-pakken i din `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Bruk den deretter i programmet ditt som følger:

```dart
// Bruker 'args'-pakken
import 'package:args/args.dart';

void main(List<String> argumenter) {
  final parser = ArgParser()..addOption('navn', abbr: 'n');
  final argResults = parser.parse(argumenter);

  if (argResults.wasParsed('navn')) {
    print('Hei, ${argResults['navn']}!');
  } else {
    print('Ingen navn oppgitt.');
  }
}
```

Kjør programmet med et navngitt argument:

```shell
dart run main.dart --name=John
```

Forventet output:

```
Hei, John!
```

Denne enkle introduksjonen til parsing av kommandolinjeargumenter, både på en nativ måte og med `args`-biblioteket, viser hvordan Dart kan håndtere brukerinndata direkte fra konsollen, noe som åpner en vei til å skape mer interaktive og dynamiske CLI-applikasjoner.
