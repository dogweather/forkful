---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:44.406075-07:00
description: "Att l\xE4sa kommandoradens argument i Dart m\xF6jligg\xF6r f\xF6r programmerare\
  \ att direkt mata in data i konsolen n\xE4r ett Dart-program k\xF6rs, vilket f\xF6\
  rb\xE4ttrar dess\u2026"
lastmod: '2024-03-13T22:44:37.629005-06:00'
model: gpt-4-0125-preview
summary: "Att l\xE4sa kommandoradens argument i Dart m\xF6jligg\xF6r f\xF6r programmerare\
  \ att direkt mata in data i konsolen n\xE4r ett Dart-program k\xF6rs, vilket f\xF6\
  rb\xE4ttrar dess\u2026"
title: "L\xE4sa argument fr\xE5n kommandoraden"
weight: 23
---

## Vad & Varför?

Att läsa kommandoradens argument i Dart möjliggör för programmerare att direkt mata in data i konsolen när ett Dart-program körs, vilket förbättrar dess interaktivitet och flexibilitet för olika användningsområden, inklusive automatiseringsskript, CLI-verktyg eller batchbehandling. Denna funktion är avgörande för att skapa anpassningsbara och användarvänliga kommandoradsapplikationer.

## Hur man gör:

Dart tillhandahåller ett enkelt sätt att komma åt kommandoradens argument via `List<String> args` i huvudmetoden. Nedan följer ett enkelt exempel som visar hur man läser och använder kommandoradens argument.

```dart
// main.dart
void main(List<String> args) {
  print('Kommandoradens Argument:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

För att köra detta Dart-program och skicka kommandoradens argument, använd Dart CLI på följande sätt:

```shell
dart run main.dart Hello World!
```

Förväntad utskrift:

```
Kommandoradens Argument:
1: Hello
2: World!
```

### Använda ett populärt tredjepartsbibliotek: `args`

Även om Darts inbyggda funktioner för hantering av kommandoradens argument är robusta för många applikationer, erbjuder `args`-paketet ett raffinerat sätt att definiera och tolka kommandoradens argument för mer komplexa behov.

Först, lägg till `args`-paketet i din `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Använd sedan det i ditt program på följande sätt:

```dart
// Använda 'args'-paketet
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hej, ${argResults['name']}!');
  } else {
    print('Inget namn angivet.');
  }
}
```

Kör programmet med ett namngivet argument:

```shell
dart run main.dart --name=John
```

Förväntad utskrift:

```
Hej, John!
```

Denna enkla introduktion till att tolka kommandoradens argument, både med inbyggda funktioner och med `args`-biblioteket, visar hur Dart kan hantera användarinmatningar direkt från konsolen, vilket öppnar en väg för att skapa mer interaktiva och dynamiska CLI-applikationer.
