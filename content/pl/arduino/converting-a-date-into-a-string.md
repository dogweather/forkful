---
title:    "Arduino: Konwersja daty na ciąg znaków"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego warto konwertować datę na łańcuch znaków?

Konwersja daty na łańcuch znaków jest ważna dla wielu projektów Arduino, gdzie potrzebujemy wyświetlić aktualną datę lub czas. Dzięki temu można stworzyć bardziej interaktywne i przydatne urządzenia IoT.

# Jak to zrobić?

Konwersja daty na łańcuch znaków w Arduino może być łatwo wykonana przy użyciu funkcji `sprintf()`. Najpierw musimy zdefiniować zmienną typu `struct tm` przechowującą aktualną datę i czas, a następnie użyć funkcji `sprintf()` do przekonwertowania jej na łańcuch znaków.

```Arduino
#include <TimeLib.h> //biblioteka Time
#include <stdio.h> //na potrzeby funkcji sprintf()

void setup() {
  //ustawiamy połączenie z monitorem szeregowym
  Serial.begin(9600);
  
  //definiujemy strukturę przechowującą datę i czas
  struct tm currentTime;
  
  //ustawiamy aktualną datę
  currentTime.tm_year = 2019;
  currentTime.tm_mon = 7;
  currentTime.tm_mday = 19;
  
  //ustawiamy aktualny czas
  currentTime.tm_hour = 12;
  currentTime.tm_min = 30;
  currentTime.tm_sec = 0;
  
  //przekonwertowanie daty i czasu na łańcuch znaków
  char dateString[30];
  sprintf(dateString, "%d/%d/%d %d:%d:%d", currentTime.tm_year,currentTime.tm_mon, currentTime.tm_mday, currentTime.tm_hour, currentTime.tm_min, currentTime.tm_sec);
  
  //wyświetlamy łańcuch znaków w monitorze szeregowym
  Serial.print(dateString);
}

void loop() {
  //puste pętle
}
```

Po wgraniu powyższego kodu na płytkę Arduino, w monitorze szeregowym powinna pojawić się skonwertowana data w formacie `YYYY/MM/DD HH:MM:SS`.

# Głębszy zanurzenie

Podstawową strukturą do przechowywania daty i czasu w Arduino jest `struct tm`, która posiada wiele innych użytecznych pól, takich jak dzień tygodnia czy numer dnia w roku. Warto zapoznać się z dokumentacją biblioteki Time w celu wykorzystania wszystkich możliwości konwersji daty i czasu.

# Zobacz także

- [Dokumentacja biblioteki Time](https://www.arduino.cc/en/Tutorial/time)
- [Przykładowy projekt wykorzystujący konwersję daty i czasu](https://create.arduino.cc/projecthub/elektropepper/display-the-date-and-time-with-arduino-c99617)