---
title:                "Arduino: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przeszłości lub przyszłości może być użyteczne w wielu projektach z użyciem Arduino. Na przykład, można wykorzystać je do zaprogramowania alarmu, który będzie działał tylko w wybranym czasie lub do kontrolowania czasu trwania projektu.

## Jak to zrobić

Aby obliczyć datę w przeszłości lub przyszłości za pomocą Arduino, wystarczy skorzystać z wbudowanej biblioteki "Time". Poniżej przedstawiony jest kod, który oblicza datę 10 dni w przyszłości od dzisiaj:

```Arduino
#include <Time.h>

void setup() {
  // inicjuj czas
  setTime(14, 0, 0, 1, 1, 2019);
  
  // pobierz aktualny czas
  time_t now = now();
  
  // dodać 10 dni do aktualnego czasu
  time_t future = now + (10 * 24 * 60 * 60);
  
  // ustaw format i wyświetl przyszłą datę
  Serial.begin(9600);
  Serial.print(day(future));
  Serial.print("/");
  Serial.print(month(future));
  Serial.print("/");
  Serial.print(year(future));
}

void loop() {}
```

Wysłany zostanie następujący wynik: "11/1/2019", oznaczający datę 10 dni od dzisiaj.

## Pogłębione badanie

Aby lepiej zrozumieć, jak działa obliczanie daty w przeszłości lub przyszłości w Arduino, warto zapoznać się z kolekcją funkcji dostępnych w bibliotece "Time". Dzięki nim możemy uzyskać dostęp do różnych części daty, takich jak dzień, miesiąc czy rok. Ponadto, można także zmieniać ustawienia czasu, takie jak strefa czasowa czy format wyświetlania.

## Zobacz też

- [Dokumentacja biblioteki "Time" dla Arduino](https://www.arduino.cc/en/Reference/Time)
- [Przykładowy projekt z użyciem obliczania daty w przeszłości i przyszłości](https://create.arduino.cc/projecthub/ninjaCoders/arduino-date-time-168b82)