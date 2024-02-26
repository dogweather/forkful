---
date: 2024-01-20 17:36:03.527608-07:00
description: "Konwersja daty na ci\u0105g znak\xF3w (string) pozwala na \u0142atwe\
  \ wy\u015Bwietlanie i zapisywanie dat w czytelnej formie. Programi\u015Bci wykonuj\u0105\
  \ t\u0119 operacj\u0119, by poprawi\u0107\u2026"
lastmod: '2024-02-25T18:49:34.054692-07:00'
model: gpt-4-1106-preview
summary: "Konwersja daty na ci\u0105g znak\xF3w (string) pozwala na \u0142atwe wy\u015B\
  wietlanie i zapisywanie dat w czytelnej formie. Programi\u015Bci wykonuj\u0105 t\u0119\
  \ operacj\u0119, by poprawi\u0107\u2026"
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Konwersja daty na ciąg znaków (string) pozwala na łatwe wyświetlanie i zapisywanie dat w czytelnej formie. Programiści wykonują tę operację, by poprawić interakcję z użytkownikiem oraz ułatwić logowanie i debugowanie programów.

## How to: (Jak to zrobić:)
Arduino nie ma wbudowanej obsługi dat, ale można użyć biblioteki `TimeLib.h`. Oto przykład:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 30, 0, 4, 1, 2021); // godzina 10:30, 4 stycznia 2021
}

void loop() {
  char buffer[20];
  sprintf(buffer, "%02d/%02d/%04d %02d:%02d:%02d", day(), month(), year(), hour(), minute(), second());
  Serial.println(buffer);
  delay(1000); // czeka 1 sekundę
}
```

Sample output:
```
04/01/2021 10:30:00
```

## Deep Dive (Szczegóły)
Historia obsługi czasu w Arduino jest mocno związana z ograniczeniami sprzętowymi mikrokontrolerów. Biblioteka `TimeLib.h` jest nieoficjalnym standardem do zarządzania czasem w Arduino, ale pamiętaj, że do utrzymywania czasu rzeczywistego potrzebny jest zegar RTC (Real Time Clock).

Alternatywą do `sprintf` jest konkatencja stringów za pomocą operatora `+`, ale to mniej wydajne i mniej elastyczne. Można też użyć `String` obiektów, które są bardziej elastyczne, ale mają większy overhead.

Kiedy konwertujesz datę na string, rozważ format – w różnych częściach świata daty przedstawiane są inaczej. 

## See Also (Zobacz Również)
- Dokumentacja Arduino do obsługi stringów: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Biblioteka `TimeLib.h`: https://www.pjrc.com/teensy/td_libs_Time.html
- Informacje o modułach RTC dla Arduino: https://www.arduino.cc/en/Reference/RTC
