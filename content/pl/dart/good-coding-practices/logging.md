---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:25.149325-07:00
description: "Logowanie w Dart odnosi si\u0119 do procesu rejestrowania r\xF3\u017C\
  nych poziom\xF3w informacji podczas wykonywania programu. Programi\u015Bci robi\u0105\
  \ to, aby monitorowa\u0107\u2026"
lastmod: '2024-03-09T21:05:59.833728-07:00'
model: gpt-4-0125-preview
summary: "Logowanie w Dart odnosi si\u0119 do procesu rejestrowania r\xF3\u017Cnych\
  \ poziom\xF3w informacji podczas wykonywania programu. Programi\u015Bci robi\u0105\
  \ to, aby monitorowa\u0107\u2026"
title: Rejestrowanie
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie w Dart odnosi się do procesu rejestrowania różnych poziomów informacji podczas wykonywania programu. Programiści robią to, aby monitorować zachowanie oprogramowania, diagnozować problemy oraz analizować wydajność, co ułatwia utrzymanie i ulepszanie aplikacji w czasie.

## Jak to zrobić:

Dart zawiera prosty mechanizm logowania poprzez bibliotekę `dart:developer`. Dla bardziej zaawansowanych potrzeb w zakresie logowania, programiści często zwracają się ku bibliotekom stron trzecich, takim jak `logger` i `log4dart`.

### Korzystanie z `dart:developer`
Jest to odpowiednie dla podstawowego logowania, szczególnie podczas rozwoju:

```dart
import 'dart:developer';

void main() {
  log('To jest komunikat logu debugowania.');
}
```

Wyjście:
```
To jest komunikat logu debugowania.
```

### Korzystanie z pakietu `logger`
Dla bardziej kompleksowego rozwiązania, pakiet `logger` oferuje różne poziomy logowania (np. informacja, ostrzeżenie, błąd) i może być formatowany w bardziej czytelny sposób.

Najpierw dodaj zależność `logger` w pliku `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

Następnie użyj go w następujący sposób:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("To jest komunikat debugowania");
  logger.w("To jest komunikat ostrzegawczy");
  logger.e("To jest komunikat o błędzie");
}
```

Przykładowe wyjście może wyglądać tak, przy czym każdy typ komunikatu jest formatowany inaczej dla łatwej identyfikacji:

```
💬 To jest komunikat debugowania
⚠️ To jest komunikat ostrzegawczy
❗️ To jest komunikat o błędzie
```

### Korzystanie z pakietu `log4dart`
Dla aplikacji wymagających logowania opartego na konfiguracji (podobnie do Log4j), `log4dart` oferuje znane podejście. Jest to szczególnie przydatne dla aplikacji na dużą skalę.

Upewnij się, że dołączyłeś `log4dart` w swoim `pubspec.yaml`:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Prosty przykład użycia:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MojaAplikacja");
  logger.debug("Debugowanie MojaAplikacja");
  logger.info("Komunikat informacyjny");
}
```

Wyjście:

```
DEBUG: Debugowanie MojaAplikacja
INFO: Komunikat informacyjny
```

Każda z tych metod zapewnia inni poziom elastyczności i złożoności, od prostych komunikatów debugowania do kompleksowego, konfigurowalnego logowania dostosowanego do potrzeb złożonych aplikacji.
