---
title:                "Interpolacja ciągu znaków"
date:                  2024-03-08T21:54:53.009756-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja łańcuchów znaków to proces wstrzykiwania wartości zmiennych bezpośrednio w łańcuchy znaków, często w celu tworzenia znaczących komunikatów bez uciążliwych konkatenacji. Programiści stosują to, aby kod był czystszy, bardziej czytelny oraz aby zapobiec błędom, które mogą wystąpić przy skomplikowanych konkatenacjach łańcuchów znaków.

## Jak to zrobić:

W Dart, interpolacja łańcuchów znaków jest prosta, wykorzystuje symbol `$` do bezpośredniej interpolacji wyrażeń w literałach łańcuchowych:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Prosta interpolacja zmiennej
  print('Uczę się $name w roku $year!');
  // Wyjście: Uczę się Dart w roku 2023!
  
  // Interpolowanie wyrażeń
  print('Za dwa lata będzie ${year + 2}.');
  // Wyjście: Za dwa lata będzie 2025.
}
```

W przypadku, kiedy masz bardziej skomplikowane wyrażenia lub chcesz wykonać operacje w samym łańcuchu, zamknij wyrażenie w `${}`. Dart nie posiada żadnych popularnych bibliotek stron trzecich specjalnie dla interpolacji łańcuchów, ponieważ jest dobrze wyposażony natywnie do obsługi zróżnicowanych i skomplikowanych scenariuszy.
