---
date: 2024-01-20 17:37:48.703655-07:00
description: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich wielkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki. Robimy\
  \ to, by ujednolici\u0107 dane przed\u2026"
lastmod: '2024-03-13T22:44:35.659401-06:00'
model: gpt-4-1106-preview
summary: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich wielkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## What & Why? (Co i dlaczego?)
Konwersja łańcucha znaków na małe litery oznacza zmianę wszystkich wielkich liter w tekście na ich małe odpowiedniki. Robimy to, by ujednolicić dane przed ich porównaniem lub przetworzeniem, zwłaszcza gdy wielkość liter nie ma znaczenia.

## How to: (Jak to zrobić:)
```Arduino
void setup() {
  Serial.begin(9600);
  
  String tekst = "Arduino Jest Fajne!";
  String tekstMalymiLiterami = tekst.toLowerCase();

  Serial.println(tekstMalymiLiterami); // arduino jest fajne!
}

void loop() {
  // Tutaj nic nie umieszczamy, wszystko dzieje się w setup.
}
```

## Deep Dive (Dogłębna analiza)
Historia funkcji zmiany na małe litery sięga początków programowania, gdy zauważono potrzebę normalizacji tekstów do porównań czy wyszukiwań. Na Arduino, używamy metody `toLowerCase()`, dostępnej w klasie `String`, która to przekształca każdą wielką literę na małą. Alternatywą jest ręczne przejście przez każdy znak i wykorzystanie funkcji `tolower()` z biblioteki `ctype.h` dla języka C, ale nie ma to sensu w przypadku prostych zastosowań Arduino.

## See Also (Zobacz również)
- Dokumentacja Arduino na temat String: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Informacje o bibliotece `ctype.h` w C: [https://en.cppreference.com/w/c/string/byte/tolower](https://en.cppreference.com/w/c/string/byte/tolower)
