---
title:                "Skriva ut felsökningsdata"
date:                  2024-03-08T21:55:30.433254-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut felsökningsutdata i Dart handlar om att visa information i konsolen under körning, vilket tillåter utvecklare att följa exekveringsflödet, undersöka variablernas tillstånd eller identifiera källan till fel. Programmerare använder det vanligtvis för felsökning och för att verifiera att deras kod beter sig som förväntat, vilket underlättar en smidigare och mer effektiv utvecklingsprocess.

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
