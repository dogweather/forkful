---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:11.081898-07:00
description: "Kapitalizacja ci\u0105gu polega na przekszta\u0142ceniu pierwszego znaku\
  \ ka\u017Cdego s\u0142owa w ci\u0105gu na wielk\u0105 liter\u0119, zapewniaj\u0105\
  c jednocze\u015Bnie, \u017Ce pozosta\u0142e litery\u2026"
lastmod: 2024-02-19 22:04:54.798038
model: gpt-4-0125-preview
summary: "Kapitalizacja ci\u0105gu polega na przekszta\u0142ceniu pierwszego znaku\
  \ ka\u017Cdego s\u0142owa w ci\u0105gu na wielk\u0105 liter\u0119, zapewniaj\u0105\
  c jednocze\u015Bnie, \u017Ce pozosta\u0142e litery\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja ciągu polega na przekształceniu pierwszego znaku każdego słowa w ciągu na wielką literę, zapewniając jednocześnie, że pozostałe litery pozostaną małe. Operacja ta jest powszechna w formacie danych i normalizacji wprowadzania użytkownika, aby zachować spójność i poprawić czytelność.

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
