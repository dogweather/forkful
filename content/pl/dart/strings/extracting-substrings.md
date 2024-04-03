---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:35.996567-07:00
description: "Wyodr\u0119bnianie podci\u0105g\xF3w dotyczy pozyskiwania okre\u015B\
  lonych cz\u0119\u015Bci ci\u0105gu na podstawie ich pozycji lub wzorc\xF3w. Programi\u015B\
  ci robi\u0105 to w celu zada\u0144 takich jak\u2026"
lastmod: '2024-03-13T22:44:35.078291-06:00'
model: gpt-4-0125-preview
summary: "Wyodr\u0119bnianie podci\u0105g\xF3w dotyczy pozyskiwania okre\u015Blonych\
  \ cz\u0119\u015Bci ci\u0105gu na podstawie ich pozycji lub wzorc\xF3w."
title: "Wydobywanie podci\u0105g\xF3w"
weight: 6
---

## Co i dlaczego?
Wyodrębnianie podciągów dotyczy pozyskiwania określonych części ciągu na podstawie ich pozycji lub wzorców. Programiści robią to w celu zadań takich jak analizowanie danych wejściowych użytkownika, manipulacja danymi, lub ekstrahowanie istotnych informacji z większych źródeł tekstu.

## Jak to zrobić:
W Dart można używać różnych metod do wyodrębniania podciągów, takich jak `substring()`, `split()`, i wyrażenia regularne. Każda metoda służy innym celom i oferuje elastyczność w obsłudze ciągów.

### Korzystanie z `substring()`:
Metoda `substring()` jest prosta. Określasz indeks początkowy (i opcjonalnie, końcowy) aby wyciąć część ciągu.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // Wynik: World
}
```

### Korzystanie z `split()`:
Podziel ciąg na listę podciągów na podstawie wzorca (takiego jak spacja lub przecinek), a następnie uzyskaj dostęp do podciągu przez indeks.

```dart
void main() {
  String example = "Dart jest fajny";
  List<String> parts = example.split(' ');
  String result = parts[1]; // Dostęp przez indeks
  print(result); // Wynik: jest
}
```

### Korzystanie z wyrażeń regularnych:
Dla skomplikowanych wzorców, klasa `RegExp` w Dart jest potężna. Użyj jej do dopasowania wzorców i wyodrębnienia podciągów.

```dart
void main() {
  String example = "Email: przyklad@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // Wynik: przyklad@mail.com
}
```

### Biblioteki stron trzecich:
Chociaż standardowa biblioteka Dart jest dość zdolna, możesz napotkać scenariusze, w których biblioteka stron trzecich mogłaby uprościć twoje zadanie. Popularnym wyborem do manipulacji ciągami i dopasowywania wzorców nie jest tutaj specjalnie promowany, jako że wbudowane możliwości Darta często wystarczają. Jednakże zawsze sprawdzaj [pub.dev](https://pub.dev) na wszelkie biblioteki, które mogłyby lepiej odpowiadać na twoje konkretne potrzeby.
