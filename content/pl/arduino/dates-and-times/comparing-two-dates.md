---
date: 2024-01-20 17:32:29.281269-07:00
description: "How to: (Jak to zrobi\u0107:) W Arduino u\u017Cywamy struktury `tmElements_t`\
  \ z biblioteki TimeLib do przechowywania dat. Por\xF3wnuj\u0105c dwa obiekty, sprawdzamy\
  \ ich\u2026"
lastmod: '2024-04-05T21:53:37.108731-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W Arduino u\u017Cywamy struktury `tmElements_t` z\
  \ biblioteki TimeLib do przechowywania dat."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## How to: (Jak to zrobić:)
W Arduino używamy struktury `tmElements_t` z biblioteki TimeLib do przechowywania dat. Porównując dwa obiekty, sprawdzamy ich kolejność.

```arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);

  tmElements_t date1;
  tmElements_t date2;

  // Ustawiamy daty
  date1.Year = 2023 - 1970;
  date1.Month = 3;
  date1.Day = 25;

  date2.Year = 2023 - 1970;
  date2.Month = 6;
  date2.Day = 15;

  // Porównujemy daty
  if (makeTime(date1) < makeTime(date2)) {
    Serial.println("Data1 jest wcześniejsza niż Data2.");
  } else if (makeTime(date1) > makeTime(date2)) {
    Serial.println("Data1 jest późniejsza niż Data2.");
  } else {
    Serial.println("Daty są identyczne.");
  }
}

void loop() {
  // Nie jest potrzebny w tym przykładzie
}
```

Sample output (Przykładowe wyjście):
```
Data1 jest wcześniejsza niż Data2.
```

## Deep Dive (Dogłębna analiza)
Porównywanie dat w komputerach sięga początków programowania. W Arduino wykorzystuje się bibliotekę TimeLib, ale są inne, jak RTClib dla modułów czasu rzeczywistego. Szczegółowe porównanie wymaga uwzględnienia czasu letniego/zimowego oraz stref czasowych, co nie jest bezpośrednio obsługiwane przez TimeLib i wymaga dodatkowej logiki.

## See Also (Zobacz także)
- TimeLib library: https://www.pjrc.com/teensy/td_libs_Time.html
- RTClib library: https://github.com/adafruit/RTClib
- Zarządzanie czasem w Arduino: https://www.arduino.cc/reference/en/libraries/time/
