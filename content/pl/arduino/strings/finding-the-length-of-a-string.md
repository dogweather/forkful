---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:46:38.846670-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

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
