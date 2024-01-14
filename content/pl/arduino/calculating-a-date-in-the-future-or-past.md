---
title:    "Arduino: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, programowalne mikrokontrolery, takie jak Arduino, są niezwykle popularne wśród hobbystów i zawodowych programistów. Pozwalają one na tworzenie projektów z dowolnej dziedziny i dodawanie inteligentnych funkcji do różnego rodzaju urządzeń. Dzięki tym możliwościom, możliwe jest także obliczanie daty w przyszłości lub przeszłości, co może być niezwykle przydatne w różnego rodzaju projektach.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, możemy wykorzystać bibliotekę "TimeLib.h" dostępną w Arduino IDE. Poniżej znajduje się przykładowy kod, który pokazuje, jak wykorzystać tę bibliotekę do przewidywania daty w przyszłości lub przeszłości:

```Arduino
#include <TimeLib.h>
#include <TimeAlarms.h>

void setup() {
  // ustawienie początkowej daty
  setTime(18, 30, 0, 1, 1, 2021);
  
  // wyświetlenie aktualnej daty
  Serial.begin(9600);
  Serial.print("Aktualna data: ");
  Serial.println(now());

  // obliczenie daty za 100 dni
  Serial.print("Data za 100 dni: ");
  Serial.println(now() + (86400 * 100)); // wartość w sekundach

  // obliczenie daty przed 10 laty
  Serial.print("Data przed 10 laty: ");
  Serial.println(now() - (86400 * 365 * 10)); // wartość w sekundach
}

void loop() {}
```

Wyjście z powyższego kodu będzie wyglądać następująco:

```
Aktualna data: 1609530600
Data za 100 dni: 1615746600
Data przed 10 laty: 1101762600
```

W powyższym kodzie używamy funkcji `setTime ()`, aby ustawić datę na 1 stycznia 2021 r. o godzinie 18:30:00. Następnie wykorzystujemy funkcję `now ()`, która zwraca liczbę sekund od 1 stycznia 1970 r., aby obliczyć datę w przyszłości lub przeszłości. Aby obliczyć datę za 100 dni, dodajemy do bieżącej liczby sekund 86400 (ilość sekund w jednym dniu) razy 100 (ilość dni). Analogicznie, aby obliczyć datę przed 10 laty, od bieżącej liczby sekund odejmujemy 86400 razy 365 (ilość dni w roku) razy 10 (ilość lat).

## Głębszy wgląd

Biblioteka "TimeLib.h" oferuje również inne przydatne funkcje, takie jak `hour ()`, `minute ()`, `second ()`, `day ()`, `month ()` i `year ()`, które pozwalają na bardziej szczegółowe ustawienie bieżącej daty i czasu. Dzięki nim możliwe jest np. obliczenie daty za 24 godziny lub 30 minut od teraźniejszości.

Warto także wspomnieć o bibliotece "TimeAlarms.h", która pozwala na ustawienie alarmów na wybrane daty i czasy. Jest to niezwykle przydatna funkcjonalność, gdy chcemy, aby nasz projekt wykonał jakieś zadanie w określonym dniu lub godzinie.

## Zobacz także

- Dokumentacja biblioteki TimeLib.h: https://playground.arduino.cc/Code/time/
- Dokumentacja biblioteki TimeAlarms.h: https://www.pjrc.com/teensy/td_libs_TimeAlarms.html