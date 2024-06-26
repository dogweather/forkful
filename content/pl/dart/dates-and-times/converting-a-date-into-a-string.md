---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:56.011394-07:00
description: "Jak to zrobi\u0107: Dart dostarcza klas\u0119 `DateTime` do obs\u0142\
  ugi dat i czas\xF3w, oraz pakiet `intl` do formatowania. Najpierw, upewnij si\u0119\
  , \u017Ce masz pakiet `intl`,\u2026"
lastmod: '2024-03-13T22:44:35.104264-06:00'
model: gpt-4-0125-preview
summary: "Dart dostarcza klas\u0119 `DateTime` do obs\u0142ugi dat i czas\xF3w, oraz\
  \ pakiet `intl` do formatowania."
title: "Konwersja daty na ci\u0105g znak\xF3w"
weight: 28
---

## Jak to zrobić:
Dart dostarcza klasę `DateTime` do obsługi dat i czasów, oraz pakiet `intl` do formatowania. Najpierw, upewnij się, że masz pakiet `intl`, dodając `intl: ^0.17.0` (lub najnowszą wersję) do pliku `pubspec.yaml`.

### Korzystanie z biblioteki podstawowej Darta
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Wyjście: 2023-4-12 (na przykład, to zależy od aktualnej daty)
```

Ten przykład bezpośrednio konstruuje łańcuch znaków z właściwości `DateTime`.

### Korzystanie z pakietu `intl`
Najpierw zaimportuj pakiet:

```dart
import 'package:intl/intl.dart';
```

Następnie sformatuj datę:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Wyjście: 2023-04-12
```

Pakiet `intl` pozwala na znacznie bardziej skomplikowane formatowanie w prosty sposób, włącznie z formatami specyficznymi dla lokalizacji:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Wyjście: April 12, 2023
```

Te przykłady pokazują proste, ale potężne sposoby konwertowania i formatowania dat na łańcuchy znaków w Darcie, zarówno za pomocą podstawowych funkcjonalności Darta, jak i wykorzystując pakiet `intl` do bardziej zaawansowanych opcji formatowania.
