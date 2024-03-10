---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.024526-07:00
description: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w (String) w Dart\
  \ polega na okre\u015Bleniu liczby jednostek kodu (w zasadzie liczby znak\xF3w,\
  \ je\u015Bli my\u015Ble\u0107 o tym uproszczonymi\u2026"
lastmod: '2024-03-09T21:05:59.816464-07:00'
model: gpt-4-0125-preview
summary: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w (String) w Dart polega\
  \ na okre\u015Bleniu liczby jednostek kodu (w zasadzie liczby znak\xF3w, je\u015B\
  li my\u015Ble\u0107 o tym uproszczonymi\u2026"
title: "Znajdowanie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków (String) w Dart polega na określeniu liczby jednostek kodu (w zasadzie liczby znaków, jeśli myśleć o tym uproszczonymi kategoriami) w danym ciągu znaków. Programiści robią to, aby precyzyjniej manipulować ciągami znaków, na przykład sprawdzając prawidłowość danych wejściowych, skracając tekst wyświetlany, lub przetwarzając formaty danych, w których długość ma znaczenie (np. protokoły z wiadomościami o określonej długości).

## Jak to zrobić:
Dart umożliwia łatwe uzyskanie długości ciągu znaków, używając właściwości `length`. Oto podstawowy przykład:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("Długość '\(myString)' wynosi: \(myString.length)");
  // Wynik: Długość 'Hello, Dart!' wynosi: 12
}
```
Ta właściwość liczy liczbę jednostek kodu UTF-16 w ciągu znaków, co odpowiada długości ciągu znaków w większości typowych przypadków użycia.

Do bardziej zniuansowanego przetwarzania tekstu, zwłaszcza z udziałem znaków Unicode spoza Podstawowej Wielojęzycznej Płaszczyzny (BMP), rozważ użycie pakietu `characters` do liczenia klastrów grafemów, które dokładniej reprezentują znaki postrzegane przez użytkownika.

Najpierw dodaj `characters` do swojego `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Następnie użyj go w ten sposób:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("Długość '\(myEmojiString)' wynosi: \(myEmojiString.characters.length)");
  // Wynik: Długość '👨‍👩‍👧‍👦 family' wynosi: 8
}
```

W tym przykładzie `myEmojiString.characters.length` podaje nam długość w terminach klastrów grafemów Unicode, co jest bardziej dokładnym odzwierciedleniem dla ciągów znaków zawierających złożone znaki, takie jak emotikony czy połączone znaki diakrytyczne.
