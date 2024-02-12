---
title:                "Konwersja ciągu znaków na małe litery"
aliases:
- /pl/arduino/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:48.703655-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
