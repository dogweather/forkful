---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.009756-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w to proces wstrzykiwania\
  \ warto\u015Bci zmiennych bezpo\u015Brednio w \u0142a\u0144cuchy znak\xF3w, cz\u0119\
  sto w celu tworzenia znacz\u0105cych komunikat\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.074603-06:00'
model: gpt-4-0125-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w to proces wstrzykiwania warto\u015B\
  ci zmiennych bezpo\u015Brednio w \u0142a\u0144cuchy znak\xF3w, cz\u0119sto w celu\
  \ tworzenia znacz\u0105cych komunikat\xF3w bez uci\u0105\u017Cliwych konkatenacji."
title: "Interpolacja ci\u0105gu znak\xF3w"
weight: 8
---

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
