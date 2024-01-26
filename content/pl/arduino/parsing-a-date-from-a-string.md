---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:34:29.802475-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków to proces wyciągania informacji o dacie z łańcucha tekstowego. Programiści robią to, by przetworzyć dane wejściowe użytkownika lub plików w formacie czytelnym dla maszyny.

## Jak to zrobić:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Nie można znaleźć RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();
  Serial.print("Aktualna data: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  delay(1000);
}
```
Wyjście przykładowe:
```
Aktualna data: 2023/3/15
```

## Wnikliwy Rzut Oka
Daty z ciągu znaków były parsowane od czasów wczesnych języków programowania. W Arduinio, wygodne jest używanie gotowych bibliotek jak RTClib, która załatwia trudną pracę. Alternatywą może być ręczne parsowanie z wykorzystaniem funkcji `sscanf` lub innych funkcji manipulacji tekstem, jednak to może być mniej wydajne i bardziej skomplikowane. Rzecz w tym, że mikrokontrolery, takie jak używane w Arduino, mają ograniczone zasoby, więc optymalizacja jest kluczowa.

## Zobacz także:
- Dokumentacja RTClib: https://adafruit.github.io/RTClib/html/index.html
- Przewodnik po bibliotece Time: http://www.arduino.cc/playground/Code/Time
- Arduino Reference (String functions): https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
