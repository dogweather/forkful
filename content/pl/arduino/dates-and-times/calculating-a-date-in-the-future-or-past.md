---
date: 2024-01-20 17:28:41.164098-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to\
  \ ustalenie dnia, miesi\u0105ca i roku przed lub po danym dniu. Programi\u015Bci\
  \ robi\u0105 to, by zarz\u0105dza\u0107 czasem w\u2026"
lastmod: '2024-03-13T22:44:35.684875-06:00'
model: gpt-4-1106-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to ustalenie\
  \ dnia, miesi\u0105ca i roku przed lub po danym dniu. Programi\u015Bci robi\u0105\
  \ to, by zarz\u0105dza\u0107 czasem w\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

# Obliczanie daty w przeszłości lub przyszłości na Arduino

## Czym i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to ustalenie dnia, miesiąca i roku przed lub po danym dniu. Programiści robią to, by zarządzać czasem w projektach, np. w systemach przypomnień czy logowaniu zdarzeń.

## Jak zrobić:
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(8, 29, 0, 8, 4, 2023); // ustawienie czasu na 8:29 8 kwietnia 2023
}

void loop() {
  time_t teraz = now();
  time_t przyszlosc = teraz + 7 * SECS_PER_DAY; // data za tydzień
  time_t przeszlosc = teraz - 30 * SECS_PER_DAY; // data sprzed miesiąca

  Serial.print("Teraz: ");
  pokazDate(teraz);
  Serial.print("Za tydzień: ");
  pokazDate(przyszlosc);
  Serial.print("Miesiąc temu: ");
  pokazDate(przeszlosc);

  delay(10000); // czekaj 10 sekund
}

void pokazDate(time_t czas) {
  Serial.println(day(czas));
  Serial.print("-");
  Serial.println(month(czas));
  Serial.print("-");
  Serial.println(year(czas));
  Serial.println();
}
```

## Dokładniej:
Kiedyś programiści musieli liczyć czasy samodzielnie — błąd roku 2000 to przykład. Teraz używają gotowych bibliotek jak `TimeLib.h`. Można też korzystać z `RTC` — modułów czasu rzeczywistego. Ważne są strefy czasowe i zmiany takie jak DST (Daylight Saving Time).

## Zobacz także:
- [TimeLib Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [RTC Lib](https://www.arduino.cc/en/Reference/RTC) 
- [Processing DateTime](http://playground.arduino.cc/Code/DateTime)
- [Arduino Time Library](https://github.com/PaulStoffregen/Time)
