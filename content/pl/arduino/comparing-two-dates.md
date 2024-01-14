---
title:    "Arduino: Porównywanie dwóch dat"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w programowaniu musimy porównywać dwa różne daty. Może to być potrzebne przy tworzeniu aplikacji, które wyświetlają czas lub przy planowaniu zadań w urządzeniach elektronicznych. Arduino jest popularną platformą dla hobbystów, ale również dla zaawansowanych programistów, dlatego warto poznać jak porównywać daty w tym środowisku.

## Jak to zrobić

Istnieje wiele metod porównywania dat w Arduino, ale jedną z najprostszych jest użycie biblioteki RTC (real time clock). Przykładowy kod wykorzystujący tę bibliotekę wyglądałby następująco:

```Arduino
#include <RTClib.h> //załadowanie biblioteki

RTC_DS1307 rtc; //utworzenie obiektu

void setup () {
    rtc.begin (); //inicjalizacja zegara
}

void loop () {
    DateTime now = rtc.now (); //pobranie aktualnej daty i czasu

    DateTime dt1 (2019, 04, 20); //ustawienie pierwszej daty
    DateTime dt2 (2019, 04, 22); //ustawienie drugiej daty

    if (now.isBetween (dt1, dt2)) { //sprawdzenie, czy aktualna data mieści się między dt1 i dt2
        Serial.println ("Aktualna data jest między 20 a 22 kwietnia 2019 roku.");
    } else {
        Serial.println ("Aktualna data nie mieści się między 20 a 22 kwietnia 2019 roku.");
    }

    delay (1000); //poczekaj 1 sekundę przed kolejnym sprawdzeniem daty
}

```

W wyniku działania powyższego kodu, na monitorze szeregowym (Serial Monitor) pojawi się odpowiedni komunikat w zależności od aktualnej daty. W ten prosty sposób możemy porównywać daty w Arduino.

## Głębszy zanurzenie

Porównywanie dat może być bardziej skomplikowane, gdy potrzebujemy uwzględnić różne strefy czasowe lub przesuwanie czasu (na przykład na wiosnę, gdy zmieniamy czas zimowy na letni). W takim przypadku możemy skorzystać z innych dostępnych bibliotek lub dostosować kod do swoich potrzeb. Ważne jest również uważne czytanie dokumentacji i testowanie kodu, aby uniknąć nieoczekiwanych rezultatów.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w Arduino, polecam przeczytać poniższe artykuły:

- [Tutorial: Working with Time and Date in Arduino](https://www.circuitbasics.com/how-to-set-time-and-date-in-arduino-without-external-timer/)
- [RTClib - biblioteka do obsługi zegara RTC w Arduino](https://github.com/adafruit/RTClib)
- [Dokumentacja Arduino](https://www.arduino.cc/reference/en/libraries/rtclib/)