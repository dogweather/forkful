---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:06.120246-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w Dart polega na wysy\u0142\
  aniu komunikat\xF3w o b\u0142\u0119dach i diagnostyk do oddzielnego strumienia,\
  \ r\xF3\u017Cnego od standardowego\u2026"
lastmod: '2024-03-09T21:05:59.843617-07:00'
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w Dart polega na wysy\u0142\
  aniu komunikat\xF3w o b\u0142\u0119dach i diagnostyk do oddzielnego strumienia,\
  \ r\xF3\u017Cnego od standardowego\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego błędu (stderr) w Dart polega na wysyłaniu komunikatów o błędach i diagnostyk do oddzielnego strumienia, różnego od standardowego wyjścia (stdout). Programiści robią to, aby odróżnić normalne wyjście programu od błędów lub komunikatów ostrzegawczych, co pozwala na łatwiejsze debugowanie i rejestrowanie.

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
