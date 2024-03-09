---
title:                "Konwersja daty na ciąg znaków"
date:                  2024-03-08T21:53:56.011394-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na łańcuch znaków w Dart jest częstym zadaniem, gdy potrzebujemy wyświetlić informacje o dacie i godzinie w formacie czytelnym dla człowieka, lub gdy zamierzamy serializować dane do przechowywania lub transmisji. Ten proces umożliwia łatwą reprezentację i manipulację wartościami daty i czasu w formacie, który jest zarówno zrozumiały, jak i może być dostosowany w zależności od przypadku użycia.

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
