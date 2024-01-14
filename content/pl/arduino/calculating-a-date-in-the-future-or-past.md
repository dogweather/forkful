---
title:                "Arduino: Obliczanie daty w przyszłości lub w przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub w przeszłości"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele projektów i urządzeń zależy od czasu. Od automatycznych systemów nawadniania ogrodów po inteligentne termometry, wiele urządzeń musi działać w odpowiednich momentach. Aby zapewnić dokładność i precyzję, często musimy wyliczyć daty w przyszłości lub z przeszłości. W tym artykule dowiesz się, jak to zrobić za pomocą Arduino.

## Jak to robić

Arduino oferuje kilka sposobów na wyliczanie dat w przyszłości lub przeszłości. Możesz wykorzystać bibliotekę "Time" lub napisać własną funkcję. Poniżej przedstawiam przykłady z użyciem biblioteki "Time" oraz z własną funkcją.

```
## Arduino przykład z użyciem biblioteki "Time"

#include <Time.h>

void setup() {
  Serial.begin(9600);

  // Ustawia aktualną datę i czas
  setTime(17, 30, 0, 1, 1, 2020);
}

void loop() {
  // Wyświetla aktualną datę i czas
  Serial.print("Aktualna data i czas: ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.print(year());
  Serial.print(" ");
  Serial.print(hour());
  Serial.print(":");
  if (minute() < 10) {
    Serial.print("0");
  }
  Serial.print(minute());
  Serial.print(":");
  if (second() < 10) {
    Serial.print("0");
  }
  Serial.println(second());

  // Wylicza datę w przyszłości o 2 dni
  time_t futureDate = now() + (2 * 24 * 60 * 60);
  Serial.print("Data za 2 dni: ");
  Serial.print(day(futureDate));
  Serial.print("/");
  Serial.print(month(futureDate));
  Serial.print("/");
  Serial.println(year(futureDate));

  // Wylicza datę w przeszłości o 2 lata
  time_t pastDate = now() - (2 * 365 * 24 * 60 * 60);
  Serial.print("Data sprzed 2 lat: ");
  Serial.print(day(pastDate));
  Serial.print("/");
  Serial.print(month(pastDate));
  Serial.print("/");
  Serial.println(year(pastDate));

  delay(1000);
}
```

```
## Arduino przykład z własną funkcją

#include <Time.h>

// Funkcja do wyliczenia daty w przeszłości lub przyszłości
time_t calculateDate(int days, int months, int years, bool future) {
  // Ustawia aktualną datę i czas
  setTime(17, 30, 0, 1, 1, 2020);

  // Wyliczenie daty w przyszłości lub przeszłości
  if (future) {
    return now() + (days * 24 * 60 * 60) + (months * 30 * 24 * 60 * 60) + (years * 365 * 24 * 60 * 60);
  } else {
    return now() - (days * 24 * 60 * 60) - (months * 30 * 24 * 60 * 60) - (years * 365 * 24 * 60 * 60);
  }
}

void setup() {
  Serial.begin(9600);
}

void loop() {
  // Wyświetla aktualną datę i czas
  Serial.print("Aktualna data i czas: ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.print(year());
  Serial.print(" ");
  Serial.print(hour());
  Serial.print(":");
  if (minute() < 10) {
    Serial.print("0");
  }
  Serial.print(minute());
  Serial.print(":");
  if (second() < 10) {
    Serial.print("0");
  }
  Serial.println(second());

  // Wylicza datę w przyszłości o 5 dni, 2 miesiące i 2 lata
  time_t futureDate = calculateDate(5, 2, 2, true);
  Serial.print("Data za 2 lata, 2 miesiące i 5 dni: ");
  Serial.print(day