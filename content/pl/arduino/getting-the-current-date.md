---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie aktualnej daty to proces, który pozwala twojemu programowi na poznanie teraźniejszego dnia, miesiąca i roku. Programiści korzystają z tej funkcji, aby dodawać znaczniki czasu do danych, kontrolować zdarzenia oparte na czasie i monitorować okresy aktywności.

## Jak to zrobić:
Aby uzyskać aktualną datę w Arduino, możemy korzystać z biblioteki `TimeLib.h`. Poniżej znajduje się kod źródłowy:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  time_t t = now();
  
  Serial.print("Dzisiaj jest: ");
  
  // Wyświetla dzień
  Serial.print(day(t));
  
  // Wyświetla miesiąc
  Serial.print("/");
  Serial.print(month(t));
  
  // Wyświetla rok
  Serial.print("/");
  Serial.println(year(t));

  delay(1000);
}
```
Gdy odpalisz powyższy kod, otrzymasz wynik w formacie DD/MM/RRRR, co odpowiada aktualnej dacie.

## Szeroki kontekst
Pobieranie aktualnej daty jest powszechne w programowaniu, ma swoje korzenie w początkach telekomunikacji, kiedy to była konieczność zapisywania nie tylko informacji, ale i czasu jej otrzymania. Istnieją różne metody na pobieranie daty, a wybór zależy od specyfikacji projektu. W Arduino możemy korzystać z funkcji RTC (Real Time Clock) lub połączyć się z serwerem NTP (Network Time Protocol) za pomocą Ethernet lub WiFi, aby uzyskać dokładniejszy czas. Głębsze szczegóły techniczne działania biblioteki `TimeLib.h` obejmują używanie unix timestamp dla reprezentacji czasu, co pozwala na łatwą konwersję i manipulację czasu.

## Zobacz także
1. Dokumentacja biblioteki `TimeLib.h`: https://github.com/PaulStoffregen/Time
2. Alternatywne rozwiązanie z użyciem czasu rzeczywistego (RTC): https://create.arduino.cc/projecthub/MisterBotBreak/how-to-use-a-real-time-clock-module-ds3231-bc90fe
3. Korzystanie z NTP dla uzyskania czasu przez WiFi: https://lastminuteengineers.com/esp8266-ntp-server-date-time-tutorial/