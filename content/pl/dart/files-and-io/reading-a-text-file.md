---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.878496-07:00
description: "Jak to zrobi\u0107: Podstawowa biblioteka Darta, `dart:io`, zapewnia\
  \ niezb\u0119dne funkcjonalno\u015Bci do synchronicznego lub asynchronicznego odczytu\
  \ plik\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.111352-06:00'
model: gpt-4-0125-preview
summary: "Podstawowa biblioteka Darta, `dart:io`, zapewnia niezb\u0119dne funkcjonalno\u015B\
  ci do synchronicznego lub asynchronicznego odczytu plik\xF3w tekstowych."
title: Czytanie pliku tekstowego
weight: 22
---

## Jak to zrobić:
Podstawowa biblioteka Darta, `dart:io`, zapewnia niezbędne funkcjonalności do synchronicznego lub asynchronicznego odczytu plików tekstowych. Oto jak podejść do obu metod.

**Synchronicznie:**

```dart
import 'dart:io';

void main() {
  var fileName = "ścieżka/do/twojego/pliku_tekstowego.txt";
  var file = File(fileName);

  // Odczytywanie pliku synchronicznie
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Błąd odczytu pliku: $e');
  }
}
```

**Asynchronicznie:**

Aby nie blokować programu podczas odczytywania pliku, co jest szczególnie przydatne dla dużych plików lub responsywnych aplikacji:

```dart
import 'dart:io';

void main() async {
  var fileName = "ścieżka/do/twojego/pliku_tekstowego.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Błąd odczytu pliku: $e');
  }
}
```

**Przykładowe wyjście:**

Jeśli twój plik tekstowy zawiera:

```
Hello, Dart!
```

Obie powyższe metody wyświetlą:

```
Hello, Dart!
```

**Korzystanie z biblioteki innej firmy:**

Dla dodatkowych funkcji, takich jak uproszczone operacje na plikach lub ulepszone obsługi błędów, można rozważyć biblioteki stron trzecich, takie jak `package:file`. Jednak, jak wynika z mojej ostatniej aktualizacji, bezpośrednie używanie podstawowego pakietu `dart:io`, jak pokazano powyżej, jest najbardziej powszechną i prostą metodą odczytu plików tekstowych w Dart.
