---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:05.224032-07:00
description: "Zamiana liter na wielkie (kapitalizacja) w ci\u0105gu znak\xF3w polega\
  \ na zmodyfikowaniu pierwszej litery s\u0142owa lub ca\u0142ego zdania na wielk\u0105\
  \ liter\u0119, pozostawiaj\u0105c\u2026"
lastmod: '2024-03-13T22:44:35.070694-06:00'
model: gpt-4-0125-preview
summary: "Zamiana liter na wielkie (kapitalizacja) w ci\u0105gu znak\xF3w polega na\
  \ zmodyfikowaniu pierwszej litery s\u0142owa lub ca\u0142ego zdania na wielk\u0105\
  \ liter\u0119, pozostawiaj\u0105c\u2026"
title: "Wielka litera w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Co i Dlaczego?

Zamiana liter na wielkie (kapitalizacja) w ciągu znaków polega na zmodyfikowaniu pierwszej litery słowa lub całego zdania na wielką literę, pozostawiając resztę znaków bez zmian. Programiści często używają tej techniki do formatowania danych wejściowych użytkownika lub wyświetlania tekstu, aby zapewnić spójność lub przestrzegać reguł gramatycznych w interfejsach użytkownika.

## Jak to zrobić:

### Korzystając z wbudowanych metod Dart

Dart oferuje proste i bezpośrednie metody manipulacji ciągami znaków. Aby zamienić literę na wielką w słowie lub zdaniu, zazwyczaj pobierasz pierwszy znak, konwertujesz go na wielką literę, a następnie łączysz go z resztą ciągu. Oto jak możesz to zaimplementować:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Wyjście: Hello world
}
```

### Kapitalizacja każdego słowa

Aby zamienić pierwszą literę każdego słowa w ciągu na wielką, możesz podzielić ciąg na słowa, zamienić każde z nich na wielką literę, a następnie połączyć je z powrotem:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Wyjście: Hello Dart Enthusiasts
}
```

### Korzystanie z bibliotek stron trzecich

Chociaż biblioteka standardowa Dart pokrywa podstawowe potrzeby, pewne zadania mogą być łatwiej wykonane przy użyciu pakietów stron trzecich. Popularnym wyborem dla rozszerzonych możliwości manipulacji ciągami znaków, w tym kapitalizacji, jest pakiet [`recase`](https://pub.dev/packages/recase). Po dodaniu go do pliku `pubspec.yaml` twojego projektu, możesz łatwo zamieniać litery na wielkie wśród innych funkcjonalności:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Wyjście: Hello World
}
```

Korzystając z `recase`, możesz zamieniać litery na wielkie w poszczególnych słowach, całych zdaniach, a nawet stosować inne konwencje zapisu bez manualnej obróbki transformacji ciągu znaków.
