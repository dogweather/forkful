---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:11.081898-07:00
description: "Jak to zrobi\u0107: Arduino, znane przede wszystkim z interakcji z sprz\u0119\
  tem, zawiera tak\u017Ce podstawowe mo\u017Cliwo\u015Bci manipulacji ci\u0105gami\
  \ za pomoc\u0105 obiektu\u2026"
lastmod: '2024-03-13T22:44:35.655623-06:00'
model: gpt-4-0125-preview
summary: "Arduino, znane przede wszystkim z interakcji z sprz\u0119tem, zawiera tak\u017C\
  e podstawowe mo\u017Cliwo\u015Bci manipulacji ci\u0105gami za pomoc\u0105 obiektu\
  \ `String`."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
Arduino, znane przede wszystkim z interakcji z sprzętem, zawiera także podstawowe możliwości manipulacji ciągami za pomocą obiektu `String`. Jednakże, brakuje mu bezpośredniej funkcji `capitalize`, jaką widzimy w językach wyższego poziomu. Dlatego implementujemy kapitalizację, iterując po ciągu znaków i stosując transformacje wielkości liter.

Oto podstawowy przykład bez używania bibliotek stron trzecich:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Zwróć pusty ciąg, jeśli wejście jest puste
  }
  input.toLowerCase(); // Najpierw zamień cały ciąg na małe litery
  input.setCharAt(0, input.charAt(0) - 32); // Zamień pierwszą literę na wielką
  
  // Zamień na wielkie litery te, które następują po spacji
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Wyjście: "Hello Arduino World"
}

void loop() {
  // Pusta pętla
}
```

Ten fragment kodu definiuje funkcję `capitalizeString`, która najpierw przekształca cały ciąg na małe litery, aby ujednolicić jego wielkość liter. Następnie zamienia pierwszy znak i każdy znak, który następuje po spacji, na wielką literę, efektywnie kapitalizując każde słowo w przekazanym ciągu. Zauważ, że ta podstawowa implementacja zakłada kodowanie znaków ASCII i może wymagać dostosowań dla pełnego wsparcia Unicode.

Obecnie nie ma szeroko przyjętych bibliotek stron trzecich specjalnie do manipulacji ciągami w ekosystemie Arduino, głównie z powodu jego skupienia na interakcji z sprzętem i efektywności. Jednakże, dostarczony przykład jest prostym sposobem na osiągnięcie kapitalizacji ciągów w środowisku programowania Arduino.
