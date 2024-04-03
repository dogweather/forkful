---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:53.633017-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to\
  \ powszechne zadanie dla programist\xF3w zajmuj\u0105cych si\u0119 planowaniem,\
  \ przypomnieniami lub jak\u0105kolwiek funkcj\u0105\u2026"
lastmod: '2024-03-13T22:44:35.106591-06:00'
model: gpt-4-0125-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to powszechne\
  \ zadanie dla programist\xF3w zajmuj\u0105cych si\u0119 planowaniem, przypomnieniami\
  \ lub jak\u0105kolwiek funkcj\u0105 zale\u017Cn\u0105 od oblicze\u0144 daty."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
Dart zapewnia solidne wsparcie dla manipulacji datami za pomocą klasy `DateTime`. Oto jak możesz obliczyć przyszłe lub przeszłe daty przy użyciu natywnego Darta, bez potrzeby korzystania z bibliotek innych firm.

### Obliczanie przyszłej daty
Aby obliczyć datę w przyszłości, tworzysz obiekt `DateTime` i używasz metody `add` z pożądanym czasem trwania.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // Wyjście: 2023-04-21 14:22:35.123456 (przykładowe wyjście, zależne od bieżącej daty i godziny)
```

### Obliczanie przeszłej daty
Aby obliczyć datę w przeszłości, używasz metody `subtract` na obiekcie `DateTime` z koniecznym czasem trwania.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // Wyjście: 2023-03-27 14:22:35.123456 (przykładowe wyjście, zależne od bieżącej daty i godziny)
```

### Korzystanie z bibliotek stron trzecich
Chociaż natywne możliwości Darta do manipulacji datami są potężne, może się zdarzyć, że potrzebujesz bardziej szczegółowych operacji, takich jak łatwiejsze parsowanie lub formatowanie dat, lub wykonywanie złożonych obliczeń. W takich przypadkach pakiet `time` może być bardzo przydatny.

Najpierw dodaj `time` do zależności w swoim `pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

Następnie możesz go użyć do wykonania podobnych obliczeń z lepszą czytelnością:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // Obliczanie przyszłej daty
  DateTime futureDate = today + 10.days;
  print(futureDate); // Format wyjścia: 2023-04-21 14:22:35.123456

  // Obliczanie przeszłej daty
  DateTime pastDate = today - 15.days;
  print(pastDate); // Format wyjścia: 2023-03-27 14:22:35.123456
}
```

Te przykłady ilustrują podstawowe manipulacje datami w Darcie, w tym dodawanie i odejmowanie czasu do lub od bieżącej daty, demonstrując, jak bez wysiłku daty mogą być zarządzane w aplikacjach Dart.
