---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.476213-07:00
description: "Wyszukiwanie i zamiana tekstu w j\u0119zyku Dart polega na badaniu ci\u0105\
  g\xF3w znak\xF3w w celu znalezienia okre\u015Blonych wzorc\xF3w lub sekwencji znak\xF3\
  w i zast\u0105pieniu ich\u2026"
lastmod: '2024-03-13T22:44:35.073448-06:00'
model: gpt-4-0125-preview
summary: "Wyszukiwanie i zamiana tekstu w j\u0119zyku Dart polega na badaniu ci\u0105\
  g\xF3w znak\xF3w w celu znalezienia okre\u015Blonych wzorc\xF3w lub sekwencji znak\xF3\
  w i zast\u0105pieniu ich\u2026"
title: "Wyszukiwanie i zast\u0119powanie tekstu"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zamiana tekstu w języku Dart polega na badaniu ciągów znaków w celu znalezienia określonych wzorców lub sekwencji znaków i zastąpieniu ich nową zawartością. Operacja ta jest podstawowa dla zadań takich jak walidacja danych, formatowanie wyników, analiza danych wprowadzanych przez użytkownika, a nawet manipulacja adresami URL i ścieżkami plików, co sprawia, że aplikacje są bardziej dynamiczne i lepiej odpowiadają na potrzeby użytkowników.

## Jak to zrobić:

Dart dostarcza wytrzymałe metody do wyszukiwania i zamiany tekstu bezpośrednio przez klasę `String`, bez potrzeby korzystania z zewnętrznych bibliotek. Oto jak możesz to zrobić:

### Podstawowe wyszukiwanie i zamiana

Aby wyszukać podciąg i zastąpić go innym ciągiem, możesz użyć `replaceAll`:

```dart
String sampleText = "Cześć, Dart! Dart jest świetny.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Output: Cześć, Flutter! Flutter jest świetny.
```

### Korzystanie z wyrażeń regularnych

Do bardziej złożonych potrzeb wyszukiwania i zamiany, Dart wykorzystuje wyrażenia regularne poprzez klasę `RegExp`. Pozwala to na dopasowywanie wzorców i zamianę w ciągach znaków:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Output: Dart 2024, Flutter 2024
```

Ten przykład znajduje wszystkie wystąpienia jednej lub więcej cyfr (`\d+`) w ciągu i zamienia je na "2024".

### Wyszukiwanie niezależne od wielkości liter

Aby wykonać wyszukiwanie niezależne od wielkości liter, możesz zmodyfikować konstruktor `RegExp`, aby ignorować wielkość liter:

```dart
String sampleText = "Witaj w Dart, języku programowania.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Output: Witaj w Flutter, języku programowania.
```

### Zastępowanie za pomocą funkcji

Dla dynamicznych zamienników opartych na samym dopasowaniu, Dart pozwala na przekazanie funkcji do `replaceAllMapped`. Ta funkcja może wykonywać operacje lub obliczenia na dopasowanych sekwencjach:

```dart
String sampleText = "Zwiększ 5 o 1, aby otrzymać 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Output: Zwiększ 6 o 1, aby otrzymać 7.
```

To zastępuje każdą sekwencję cyfr ich wartością zwiększoną. Każde dopasowanie jest analizowane jako liczba całkowita, zwiększane, a następnie zamieniane z powrotem na ciąg znaków dla zamiany.

Możliwości manipulacji ciągami znaków w Dart, szczególnie dla wyszukiwania i zamiany tekstu, czynią go potężnym narzędziem do przetwarzania i przygotowywania danych w ramach aplikacji. Czy to za pomocą prostych zamienników ciągów znaków czy wykorzystania mocy wyrażeń regularnych, Dart zapewnia elastyczność i wydajność potrzebną do skutecznej manipulacji tekstem.
