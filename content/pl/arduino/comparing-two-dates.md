---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Porównywanie dwóch dat to proces ustalania, która data jest wcześniejsza, a która późniejsza. Programiści wykonują to, aby zarządzać i manipulować sekwencją zdarzeń w czasie.
  
## Jak to zrobić:

Poniżej przedstawiam prosty kod umożliwiający porównanie dwóch dat w Arduino:

```Arduino
#include <TimeLib.h>

time_t data1;
time_t data2;

void setup() {
  Serial.begin(9600);
  setTime(20, 00, 00, 4, 01, 2021); //ustawienie czasu: godzina, minuta, sekunda, dzień, miesiąc, rok
  data1 = now();
  setTime(21, 30, 00, 4, 01, 2021);
  data2 = now();
}

void loop() {
  if(data1>data2) {
    Serial.println("Data1 jest późniejsza");
  } 
  else {
    Serial.println("Data2 jest późniejsza");
  }
}
```

Po uruchomieniu tego kodu w Arduino, wyjście powinno pokazać: "Data2 jest późniejsza".

## Więcej szczegółów

Porównywanie dat jest jednym z podstawowych zadań w programowaniu. Jest to szczególnie ważne w aplikacjach, które polegają na przetwarzaniu i zarządzaniu danymi czasowymi, takie jak rezerwacja bilety lotu, planowanie projektu czy zarządzanie zasobami.

Historia: Moduł TimeLib, używany w powyższym przykładzie, jest często używany do zarządzania datą i czasem w Arduino od wczesnych wersji Arduino.

Alternatywy: Istnieją inne biblioteki, takie jak DS3231, RTClib, które również umożliwiają porównywanie dat, ale mogą wymagać dodatkowego sprzętu.

Szczegóły implementacji: W Arduino, daty są przypisywane do zmiennych typu time_t. Zmienna ta przechowuje liczbę sekund, które upłynęły od 1 stycznia 1970 roku (znane jako epoki Unix). Artytmetyka dla tego typu danych jest prosta i efektywna, co pozwala na łatwe porównywanie dat.

## Zobacz też

- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- Porównywanie dat z biblioteką DS3231: https://github.com/JChristensen/DS3232RTC
- Szczegółowy przewodnik po obsłudze czasu i daty w Arduino: https://www.makerguides.com/rtc-arduino-guide/