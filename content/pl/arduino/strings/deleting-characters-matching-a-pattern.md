---
date: 2024-01-20 17:41:32.040173-07:00
description: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to odfiltrowywanie okre\u015B\
  lonych znak\xF3w z tekstu. Programi\u015Bci robi\u0105 to, \u017Ceby oczy\u015B\
  ci\u0107 dane, poprawia\u0107 format lub\u2026"
lastmod: '2024-03-13T22:44:35.656649-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to odfiltrowywanie okre\u015B\
  lonych znak\xF3w z tekstu."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić:)
```Arduino
String deleteMatchingChars(String str, String pattern) {
  String result = "";
  for (char c : str) {
    if (pattern.indexOf(c) < 0) {
      result += c;
    }
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String text = "Hello World! 1234";
  String pattern = "lo2";
  Serial.println(deleteMatchingChars(text, pattern)); // Wyświetli "He World! 134"
}

void loop() {
  // Pusta pętla - logika jest w setup
}
```

## Deep Dive (Dogłębna analiza)
Usuwanie określonych znaków nie jest nowością. Funkcje podobne do `deleteMatchingChars` istnieją w wielu językach programowania i są używane od dekad. Alternatywą jest wyrażenia regularne (regex), które oferują więcej elastyczności. W Arduino jednak używa się prostszych metod ze względu na ograniczone zasoby sprzętowe. Implementacja `indexOf` sprawdza, czy znak pasuje do wzoru - jeśli nie, dodaje go do wynikowego ciągu.

## See Also (Zobacz również)
- Dokumentacja języka Arduino String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Intrukcja obsługi `indexOf`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/
- Podstawy wyrażeń regularnych: https://www.regular-expressions.info/
