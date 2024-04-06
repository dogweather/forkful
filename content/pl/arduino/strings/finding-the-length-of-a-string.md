---
date: 2024-01-20 17:46:38.846670-07:00
description: "Jak to zrobi\u0107: Kiedy\u015B, w C, do d\u0142ugo\u015Bci string\xF3\
  w u\u017Cywaliby\u015Bmy `strlen` z biblioteki `string.h`. W Arduino, `String` to\
  \ klasa u\u0142atwiaj\u0105ca zarz\u0105dzanie\u2026"
lastmod: '2024-04-05T22:50:49.994172-06:00'
model: gpt-4-1106-preview
summary: "Kiedy\u015B, w C, do d\u0142ugo\u015Bci string\xF3w u\u017Cywaliby\u015B\
  my `strlen` z biblioteki `string.h`."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## Jak to zrobić:
```Arduino
void setup() {
  Serial.begin(9600);
  
  String mojTekst = "Czesć Świecie!";
  int dlugoscTekstu = mojTekst.length();
  
  Serial.print("Długość tekstu: ");
  Serial.println(dlugoscTekstu);
}

void loop() {
  // nic nie robimy w pętli
}
```
W Sample Output widzimy:
```
Długość tekstu: 14
```

## W Głębi Tematu
Kiedyś, w C, do długości stringów używalibyśmy `strlen` z biblioteki `string.h`. W Arduino, `String` to klasa ułatwiająca zarządzanie tekstami. Jednak używanie `String` może prowadzić do fragmentacji pamięci, więc czasem lepiej użyć `char` arrays wraz z `strlen`. `String.length()` jest prostsze, ale dobrze zastanowić się nad jej użyciem w projektach gdzie stabilność jest kluczowa.

## Zobacz Również
- [Arduino Reference for String.length()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
