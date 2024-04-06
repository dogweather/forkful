---
date: 2024-01-20 17:37:48.703655-07:00
description: "How to: (Jak to zrobi\u0107:) Historia funkcji zmiany na ma\u0142e litery\
  \ si\u0119ga pocz\u0105tk\xF3w programowania, gdy zauwa\u017Cono potrzeb\u0119 normalizacji\
  \ tekst\xF3w do por\xF3wna\u0144 czy\u2026"
lastmod: '2024-04-05T22:50:49.989939-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historia funkcji zmiany na ma\u0142e litery si\u0119\
  ga pocz\u0105tk\xF3w programowania, gdy zauwa\u017Cono potrzeb\u0119 normalizacji\
  \ tekst\xF3w do por\xF3wna\u0144 czy wyszukiwa\u0144."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

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
