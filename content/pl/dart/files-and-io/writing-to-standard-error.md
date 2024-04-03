---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:06.120246-07:00
description: "Jak to zrobi\u0107: W Dart, pisanie do stderr jest proste za pomoc\u0105\
  \ obiektu `stderr` dost\u0119pnego w `dart:io`. Oto podstawowy przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.110253-06:00'
model: gpt-4-0125-preview
summary: "W Dart, pisanie do stderr jest proste za pomoc\u0105 obiektu `stderr` dost\u0119\
  pnego w `dart:io`."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W Dart, pisanie do stderr jest proste za pomocą obiektu `stderr` dostępnego w `dart:io`. Oto podstawowy przykład:

```dart
import 'dart:io';

void main() {
  stderr.writeln('To jest komunikat o błędzie.');
}
```

Wyjście po uruchomieniu:
```
To jest komunikat o błędzie.
```
Ten komunikat jest wysyłany do strumienia stderr, który zwykle jest wyświetlany w konsoli lub terminalu.

Aby zademonstrować większą złożoność, taką jak rejestrowanie wyjątku, bogaty zestaw funkcji Darta pozwala na zwięzłe i skuteczne obsługiwanie błędów:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Symulacja operacji, która może zgłosić wyjątek
    throw Exception('Coś poszło nie tak!');
  } catch (e) {
    stderr.writeln('Błąd: $e');
  }
}

void main() {
  riskyOperation();
}
```

Wyjście po uruchomieniu:
```
Błąd: Exception: Coś poszło nie tak!
```

Ten wzorzec jest szczególnie użyteczny dla aplikacji, które muszą oddzielić normalne logi od logów błędów, ułatwiając monitorowanie i debugowanie aplikacji.

Chociaż standardowa biblioteka Darta jest dość wszechstronna, wiele programów nie wymaga bibliotek stron trzecich do pisania do stderr. Jeśli jednak twoja aplikacja wymaga bardziej zaawansowanych możliwości rejestrowania (np. do plików, przez sieć, formatowanie), pakiet `logging` jest popularnym wyborem. Oto szybki rzut oka na użycie `logging` do błędów:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Poważny błąd: Stało się coś bardzo złego.');
}
```

Wyjście po uruchomieniu:
```
SEVERE: 2023-04-01 00:00:00.000: Poważny błąd: Stało się coś bardzo złego.
```

Ta metoda oferuje większy stopień dostosowania i kontroli nad tym, co jest rejestrowane jako błąd i jak jest formatowane, co może być bardzo pomocne w większych, bardziej złożonych aplikacjach.
