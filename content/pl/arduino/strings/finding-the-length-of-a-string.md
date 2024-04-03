---
date: 2024-01-20 17:46:38.846670-07:00
description: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w to sprawdzenie,\
  \ ile znak\xF3w zawiera. Programi\u015Bci robi\u0105 to, by zarz\u0105dza\u0107\
  \ danymi tekstowymi, walidowa\u0107 wej\u015Bcia lub\u2026"
lastmod: '2024-03-13T22:44:35.663395-06:00'
model: gpt-4-1106-preview
summary: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w to sprawdzenie, ile\
  \ znak\xF3w zawiera."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## Co i Dlaczego?
Znalezienie długości ciągu znaków to sprawdzenie, ile znaków zawiera. Programiści robią to, by zarządzać danymi tekstowymi, walidować wejścia lub oszacować miejsce potrzebne do przechowywania informacji.

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
