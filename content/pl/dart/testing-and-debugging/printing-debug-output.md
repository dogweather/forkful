---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-03-08T21:55:32.162054-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wypisywanie danych diagnostycznych w Dart polega na wyświetlaniu informacji w konsoli podczas działania programu, co pozwala programistom śledzić przepływ wykonania, badać stan zmiennych lub identyfikować źródło błędów. Programiści powszechnie używają tego do rozwiązywania problemów i weryfikowania, czy ich kod zachowuje się zgodnie z oczekiwaniami, co ułatwia sprawniejszy i bardziej efektywny proces rozwijania oprogramowania.

## Jak to zrobić:

W Dart można wypisywać dane diagnostyczne za pomocą funkcji `print()`. Oto jak wypisać proste wiadomości i wartości zmiennych:

```dart
void main() {
  String greeting = "Hello, Dart!";
  print(greeting); // Wypisuje: Hello, Dart!

  int number = 42;
  print('The number is $number.'); // Wypisuje: The number is 42.
}
```

Dla strukturyzowanych danych, takich jak listy lub obiekty, metoda `toString()` w Dart może nie dostarczać wystarczających szczegółów. W takich przypadkach, można użyć funkcji `jsonEncode` z biblioteki `dart:convert` Darta, aby przekonwertować dane na ciąg JSON dla bardziej czytelnego wyjścia:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // Wypisuje: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

Gdy potrzebne są bardziej zaawansowane możliwości debugowania, takie jak logowanie z różnymi poziomami ważności (informacje, ostrzeżenia, błędy), można użyć bibliotek firm trzecich, takich jak `logger`. Oto jak tego użyć:

1. Dodaj `logger` do swojego `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Użyj `logger` w swoim kodzie Dart:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("To jest wiadomość diagnostyczna");
  logger.w("To jest wiadomość ostrzegawcza");
  logger.e("To jest wiadomość o błędzie");
}
```

Wyjście będzie bardziej informacyjne, pokazując poziom wiadomości i samą wiadomość, co ułatwia odróżnienie między różnymi rodzajami komunikatów logowania.
