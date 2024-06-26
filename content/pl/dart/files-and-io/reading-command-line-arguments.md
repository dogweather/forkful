---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:07.190118-07:00
description: "Jak to zrobi\u0107: Dart oferuje prost\u0105 metod\u0119 dost\u0119\
  pu do argument\xF3w linii polece\u0144 za po\u015Brednictwem `List<String> args`\
  \ w g\u0142\xF3wnej metodzie. Poni\u017Cej znajduje\u2026"
lastmod: '2024-03-13T22:44:35.109013-06:00'
model: gpt-4-0125-preview
summary: "Dart oferuje prost\u0105 metod\u0119 dost\u0119pu do argument\xF3w linii\
  \ polece\u0144 za po\u015Brednictwem `List<String> args` w g\u0142\xF3wnej metodzie."
title: "Czytanie argument\xF3w z linii polece\u0144"
weight: 23
---

## Jak to zrobić:
Dart oferuje prostą metodę dostępu do argumentów linii poleceń za pośrednictwem `List<String> args` w głównej metodzie. Poniżej znajduje się prosty przykład demonstrujący, jak odczytywać i wykorzystywać argumenty linii poleceń.

```dart
// main.dart
void main(List<String> args) {
  print('Argumenty linii poleceń:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Aby uruchomić ten program Dart i przekazać argumenty linii poleceń, użyj Dart CLI w następujący sposób:

```shell
dart run main.dart Hello World!
```

Oczekiwane wyjście:

```
Argumenty linii poleceń:
1: Hello
2: World!
```

### Używanie popularnej biblioteki stron trzecich: `args`
Chociaż wbudowane możliwości Darta do obsługi argumentów linii poleceń są solidne dla wielu aplikacji, pakiet `args` zapewnia udoskonalony sposób definiowania i analizowania argumentów linii poleceń dla bardziej złożonych potrzeb.

Najpierw dodaj pakiet `args` do swojego `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Następnie użyj go w swoim programie w następujący sposób:

```dart
// Używanie pakietu 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Cześć, ${argResults['name']}!');
  } else {
    print('Nie podano imienia.');
  }
}
```

Uruchom program z argumentem nazwanym:

```shell
dart run main.dart --name=John
```

Oczekiwane wyjście:

```
Cześć, John!
```

To proste wprowadzenie do analizowania argumentów linii poleceń, zarówno natywnie, jak i za pomocą biblioteki `args`, pokazuje, jak Dart może obsługiwać wejścia użytkownika bezpośrednio z konsoli, otwierając drogę do tworzenia bardziej interaktywnych i dynamicznych aplikacji CLI.
