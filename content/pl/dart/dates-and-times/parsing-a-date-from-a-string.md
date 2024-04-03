---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.066965-07:00
description: "Parsowanie daty z ci\u0105gu znak\xF3w w Dart polega na konwertowaniu\
  \ tekstowego przedstawienia dat i czas\xF3w na obiekt `DateTime`. Operacja ta jest\
  \ niezb\u0119dna dla\u2026"
lastmod: '2024-03-13T22:44:35.101924-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie daty z ci\u0105gu znak\xF3w w Dart polega na konwertowaniu tekstowego\
  \ przedstawienia dat i czas\xF3w na obiekt `DateTime`."
title: "Analiza sk\u0142adniowa daty z ci\u0105gu znak\xF3w"
weight: 30
---

## Jak to zrobić:
Podstawowa biblioteka Dart ułatwia parsowanie dat za pomocą klasy `DateTime`. W prostych przypadkach, gdy znasz format ciągu daty, możesz użyć metody `DateTime.parse()`. Jednak w bardziej skomplikowanych scenariuszach lub przy obsłudze wielu formatów, pakiet `intl`, a konkretnie klasa `DateFormat`, staje się nieoceniony.

### Korzystanie z Podstawowej Biblioteki Dart:
```dart
void main() {
  // Korzystając z DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Korzystanie z pakietu `intl`:
Najpierw dodaj pakiet `intl` do pliku `pubspec.yaml`:
```yaml
dependencies:
  intl: ^0.17.0
```
Następnie zaimportuj pakiet i użyj `DateFormat` do parsowania:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
Pakiet `intl` oferuje solidne opcje parsowania dat, umożliwiając bezproblemowe obsługiwanie różnych międzynarodowych formatów dat.
