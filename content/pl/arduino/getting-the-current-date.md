---
title:    "Arduino: Pobieranie bieżącej daty"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Często informacja o aktualnej dacie jest niezbędna do wykonywania projektów związanych z elektroniką. Na przykład może to być potrzebne do uruchomienia alarmu o określonej godzinie lub wyświetlania aktualnej daty na ekranie. W tym artykule omówimy jak w prosty sposób uzyskać aktualną datę na Arduino.

## Jak to zrobić

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;
DateTime now;

void setup() {
  Serial.begin(9600);
  rtc.begin();

  // Uzyskanie aktualnej daty i godziny z RTC
  now = rtc.now();

  // Wyswietlenie daty w formacie dd/mm/yyyy
  Serial.println(String(now.day()) + "/" + String(now.month()) + "/" + String(now.year()));

  // Wyswietlenie godziny w formacie hh:mm:ss
  Serial.println(String(now.hour()) + ":" + String(now.minute()) + ":" + String(now.second()));
}

void loop() {
}
```

Przede wszystkim musimy zainstalować bibliotekę RTClib, która pozwala na obsługę modułu RTC (Real Time Clock). Następnie, korzystając z funkcji `.begin()`, inicjalizujemy moduł RTC. W celu uzyskania aktualnej daty i godziny, używamy funkcji `.now()`, która zwraca obiekt klasy `DateTime`. Następnie możemy wyświetlić aktualną datę i godzinę w formacie, który wybierzemy. W tym przykładzie wyświetlamy je w formatach `dd/mm/yyyy` oraz `hh:mm:ss`.

## Deep Dive

Moduł RTC jest wyposażony w baterię, która pozwala na zachowanie aktualnej daty i godziny nawet w przypadku zaniku zasilania. W związku z tym, nie musimy ustawiać daty i godziny za każdym razem po ponownym uruchomieniu Arduino. Moduł RTC może również działać jako timer, umożliwiając np. wykonywanie określonych czynności w ustalonych odstępach czasu.

## Zobacz także

- Dokumentacja biblioteki RTClib: https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit
- Poradnik dotyczący korzystania z modułu RTC: https://arduinogetstarted.com/tutorials/arduino-real-time-clock-ds1307-tutorial