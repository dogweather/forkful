---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.009756-07:00
description: "Jak to zrobi\u0107: W Dart, interpolacja \u0142a\u0144cuch\xF3w znak\xF3\
  w jest prosta, wykorzystuje symbol `$` do bezpo\u015Bredniej interpolacji wyra\u017C\
  e\u0144 w litera\u0142ach \u0142a\u0144cuchowych."
lastmod: '2024-03-13T22:44:35.074603-06:00'
model: gpt-4-0125-preview
summary: "W Dart, interpolacja \u0142a\u0144cuch\xF3w znak\xF3w jest prosta, wykorzystuje\
  \ symbol `$` do bezpo\u015Bredniej interpolacji wyra\u017Ce\u0144 w litera\u0142\
  ach \u0142a\u0144cuchowych."
title: "Interpolacja ci\u0105gu znak\xF3w"
weight: 8
---

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
