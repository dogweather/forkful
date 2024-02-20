---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:04.269480-07:00
description: "Pobieranie aktualnej daty w projektach Arduino polega na uzyskiwaniu\
  \ informacji w czasie rzeczywistym, co mo\u017Ce by\u0107 kluczowe do logowania,\
  \ stemplowania\u2026"
lastmod: 2024-02-19 22:04:54.826208
model: gpt-4-0125-preview
summary: "Pobieranie aktualnej daty w projektach Arduino polega na uzyskiwaniu informacji\
  \ w czasie rzeczywistym, co mo\u017Ce by\u0107 kluczowe do logowania, stemplowania\u2026"
title: Pobieranie aktualnej daty
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie aktualnej daty w projektach Arduino polega na uzyskiwaniu informacji w czasie rzeczywistym, co może być kluczowe do logowania, stemplowania czasowego czy planowania zadań. Programiści często potrzebują tej funkcjonalności, by zwiększyć użyteczność, zapewnić aktualność danych i ułatwić operacje zależne od czasu w ich projektach IoT i wbudowanych.

## Jak to zrobić:
Arduino samo w sobie nie ma wbudowanej metody bezpośredniego pobierania aktualnej daty, ponieważ brakuje mu zegara czasu rzeczywistego (RTC). Można to jednak osiągnąć, używając zewnętrznych modułów RTC, takich jak DS3231, oraz bibliotek, np. `RTClib`, opracowanej przez Adafruit, która ułatwia współpracę z tymi modułami.

Najpierw upewnij się, że biblioteka `RTClib` jest zainstalowana w twoim środowisku Arduino IDE. Następnie podłącz moduł RTC do swojego Arduino zgodnie z jego dokumentacją.

Oto prosty przykład, żeby zacząć:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Nie znaleziono RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC stracił zasilanie, ustawmy czas!");
    // Gdy potrzeba ustawić czas na nowym urządzeniu lub po utracie zasilania, możesz to zrobić tutaj.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
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

  delay(3000); // Opóźnienie o 3 sekundy, aby ograniczyć zalew danych serialowych
}
```

Przykładowe wyjście (zakładając, że twój RTC został wcześniej ustawiony):

```
Aktualna data: 2023/4/15
```

Ten kod inicjalizuje moduł RTC, a następnie w pętli pobiera i wyświetla aktualną datę na Monitorze Szeregowym co 3 sekundy. Pamiętaj, że linię `rtc.adjust(...)` można odkomentować i zmodyfikować, aby początkowo ustawić datę i godzinę RTC lub po tym, jak stracił zasilanie.
