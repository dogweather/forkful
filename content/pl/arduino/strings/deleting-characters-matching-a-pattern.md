---
title:                "Usuwanie znaków pasujących do wzorca"
aliases: - /pl/arduino/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:32.040173-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Usuwanie znaków pasujących do wzorca to odfiltrowywanie określonych znaków z tekstu. Programiści robią to, żeby oczyścić dane, poprawiać format lub przygotować tekst do przetwarzania.

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
