---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:30.433254-07:00
description: "Att skriva ut fels\xF6kningsutdata i Dart handlar om att visa information\
  \ i konsolen under k\xF6rning, vilket till\xE5ter utvecklare att f\xF6lja exekveringsfl\xF6\
  det,\u2026"
lastmod: '2024-03-13T22:44:37.614785-06:00'
model: gpt-4-0125-preview
summary: "Att skriva ut fels\xF6kningsutdata i Dart handlar om att visa information\
  \ i konsolen under k\xF6rning, vilket till\xE5ter utvecklare att f\xF6lja exekveringsfl\xF6\
  det, unders\xF6ka variablernas tillst\xE5nd eller identifiera k\xE4llan till fel."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Hur man gör:
I Dart kan du skriva ut felsökningsutdata med `print()`-funktionen. Så här skriver du ut enkla meddelanden och variabelvärden:

```dart
void main() {
  String greeting = "Hej, Dart!";
  print(greeting); // Skriver ut: Hej, Dart!

  int number = 42;
  print('Numret är $number.'); // Skriver ut: Numret är 42.
}
```

För strukturerade data, som listor eller objekt, kanske Darts `toString()`-metod inte ger tillräckligt med detaljer. I de fallen kan du använda funktionen `jsonEncode` från Darts `dart:convert`-bibliotek för att konvertera datan till en JSON-sträng för mer läslig utdata:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // Skriver ut: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

När mer sofistikerade felsökningsfunktioner behövs, såsom loggning med olika nivåer av viktighet (info, varning, fel), kan du använda tredjepartsbibliotek som `logger`. Så här använder du det:

1. Lägg till `logger` i din `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Använd `logger` i din Dart-kod:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Det här är ett felsökningsmeddelande");
  logger.w("Det här är ett varningsmeddelande");
  logger.e("Det här är ett felmeddelande");
}
```

Utmatningen blir mer informativ, visar meddelandets nivå och meddelandet självt, vilket gör det lättare att skilja mellan olika typer av loggmeddelanden.
