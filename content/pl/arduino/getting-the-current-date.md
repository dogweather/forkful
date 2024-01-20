---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:01.334060-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i Dlaczego?)
W Arduino chodzi o dostanie aktualnej daty z czasu rzeczywistego. Programiści robią to, by śledzić zdarzenia czasowe i synchronizować działania urządzeń.

## How to:
(Jak to zrobić:)
Potrzebujesz modułu RTC (Real Time Clock), jak DS3231. Poniżej kod inicjujący RTC i wyświetlający datę:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Nie znaleziono modułu RTC!");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC utracił zasilanie, ustawiamy czas!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  delay(1000);  // Odczyt co sekundę
}
```
Wynik w Serial Monitor:
```
2023/3/15
```

## Deep Dive:
(Głębszy Wgląd:)
RTC, jak DS3231, zyskał popularność w projektach Arduino za precyzję i pamięć na baterii. Alternatywy to GPS czy Internet Time Protocol, ale są droższe lub trudniejsze w implementacji. RTC służy też do budzenia Arduino z trybu uśpienia.

## See Also:
(Zobacz Również:)
- Dokumentacja RTClib: https://github.com/adafruit/RTClib
- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- Poradnik do modułu RTC DS3231: http://www.rinkydinkelectronics.com/library.php?id=73