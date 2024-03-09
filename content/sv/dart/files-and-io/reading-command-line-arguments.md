---
title:                "Läsa argument från kommandoraden"
date:                  2024-03-08T21:55:44.406075-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
