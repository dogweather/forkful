---
date: 2024-01-20 17:36:03.527608-07:00
description: "How to: (Jak to zrobi\u0107:) Arduino nie ma wbudowanej obs\u0142ugi\
  \ dat, ale mo\u017Cna u\u017Cy\u0107 biblioteki `TimeLib.h`. Oto przyk\u0142ad."
lastmod: '2024-04-05T21:53:37.107777-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Arduino nie ma wbudowanej obs\u0142ugi dat, ale mo\u017C\
  na u\u017Cy\u0107 biblioteki `TimeLib.h`."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

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
