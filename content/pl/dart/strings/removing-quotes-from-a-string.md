---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:34.252895-07:00
description: "Jak to zrobi\u0107: Dart oferuje proste sposoby na usuni\u0119cie cudzys\u0142\
  ow\xF3w z ci\u0105gu znak\xF3w przy u\u017Cyciu wbudowanych metod ci\u0105gu znak\xF3\
  w, bez potrzeby korzystania z\u2026"
lastmod: '2024-03-13T22:44:35.077149-06:00'
model: gpt-4-0125-preview
summary: "Dart oferuje proste sposoby na usuni\u0119cie cudzys\u0142ow\xF3w z ci\u0105\
  gu znak\xF3w przy u\u017Cyciu wbudowanych metod ci\u0105gu znak\xF3w, bez potrzeby\
  \ korzystania z bibliotek firm trzecich."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Dart oferuje proste sposoby na usunięcie cudzysłowów z ciągu znaków przy użyciu wbudowanych metod ciągu znaków, bez potrzeby korzystania z bibliotek firm trzecich.

### Przykład 1: Użycie `replaceFirst` i `replaceAll`
Jeśli masz do czynienia z ciągami znaków, które zaczynają się i kończą na cudzysłowach, możesz użyć metod `replaceFirst` i `replaceAll`, aby je usunąć.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Programowanie w Dart\'';

// Usuwanie podwójnych cudzysłowów
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Wynik: Hello, World!

// Usuwanie pojedynczych cudzysłowów
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Wynik: Programowanie w Dart
```

### Przykład 2: Użycie `substring`
Ta metoda jest użyteczna, gdy jesteś pewien, że cudzysłowy znajdują się na samym początku i końcu ciągu znaków.

```dart
String quotedString = '"Rozwój Flutter"';
// Sprawdź, czy zaczyna się i kończy na cudzysłowach przed usunięciem, aby uniknąć błędów
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Wynik: Rozwój Flutter
```

### Przykład 3: Metoda rozszerzenia niestandardowego
Dla większej możliwości ponownego użycia, szczególnie jeśli Twój projekt często wymaga usuwania cudzysłowów, rozważ stworzenie niestandardowego rozszerzenia `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"To jest Dart"';
  String singleQuoted = '\'To jest niesamowite\'';
  print(doubleQuoted.unquote()); // Wynik: To jest Dart
  print(singleQuoted.unquote()); // Wynik: To jest niesamowite
}
```

Te podejścia powinny pomóc Ci skutecznie usuwać cudzysłowy z ciągów znaków w Dart, zwiększając efektywność Twoich prac nad przetwarzaniem i przygotowaniem danych.
