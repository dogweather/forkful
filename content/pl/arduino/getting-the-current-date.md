---
title:                "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego programować Arduino?
Arduino jest popularnym mikrokontrolerem, który można programować w prosty sposób za pomocą języka C++. Umożliwia on wykonanie wielu różnych zadań, w tym również pobranie aktualnej daty. Może to być przydatne dla osób, które tworzą projekty z wykorzystaniem Arduino, takie jak zegary czy kalendarze.

## Jak pobrać aktualną datę na Arduino?
Aby pobrać aktualną datę na Arduino, możemy skorzystać z funkcji ```millis()```, która zwraca liczbę milisekund od uruchomienia płytki. Dzięki temu możemy obliczyć, ile czasu minęło od konkretnej daty, np. od roku 1970. Poniższy kod pokazuje jak to zrobić:

```Arduino
unsigned long czas_od_uruchomienia = millis();
unsigned long sekundy = czas_od_uruchomienia / 1000; // zamiana milisekund na sekundy
unsigned long minuty = sekundy / 60;
unsigned long godziny = minuty / 60;
unsigned long dni = godziny / 24;
unsigned long lata = dni / 365;
```

W powyższym przykładzie, zmienna ```lata``` będzie przechowywać ilość lat od roku 1970. Aby uzyskać bardziej precyzyjny wynik, możemy dodatkowo skorzystać z modułu czasowego RTC (Real Time Clock), który jest dostępny w większości płytek Arduino.

## Głębszy zanurzenie
Istnieje kilka sposobów na uzyskanie aktualnej daty w programowaniu Arduino. W zależności od naszych potrzeb, możemy wybrać najbardziej odpowiedni sposób. Jednym z nich jest wykorzystanie modułu RTC, który ma wbudowany układ zegara i baterię, dlatego dane dotyczące daty i godziny będą przechowywane nawet po odłączeniu płytki od źródła zasilania.

Inną opcją jest wykorzystanie modułu WiFi lub Ethernet, który będzie komunikował się z serwerem NTP (Network Time Protocol) i pobierał aktualną datę i godzinę. Możemy również samodzielnie napisać kod, który będzie korzystał z sieci i serwera NTP, ale wymaga to nieco więcej pracy.

## Zobacz również
- [Biblioteka RTClib dla Arduino](https://github.com/adafruit/RTClib)
- [Przewodnik po wykorzystaniu modułu WiFi ESP8266 w Arduino](https://micropython-on-wemos-d1-mini.readthedocs.io/en/latest/setup.html)
- [Dokumentacja funkcji millis() w Arduino](https://www.arduino.cc/reference/en/language/functions/time/millis/)