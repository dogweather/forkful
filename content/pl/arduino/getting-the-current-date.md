---
title:                "Arduino: Pobieranie bieżącej daty"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Jesteś ciekawy, jak program może pobrać aktualną datę? To jeden ze sposobów, aby dowiedzieć się, jak świat elektroniki może być zintegrowany z czasem rzeczywistym.

## Jak to zrobić

Wykorzystaj funkcję "tńow" do pobrania aktualnej daty. Oto przykładowy kod dla Arduino Uno:

```arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
  setTime(12, 30, 0, 1, 1, 2018); // ustawia czas na godzinę 12:30, dzień 1, stycznia 2018 r.
}

void loop() {
  // pobiera aktualną datę i czas
  int year = year();
  int month = month();
  int day = day();
  int hour = hour();
  int minute = minute();
  int second = second();

  // wyświetla aktualną datę i czas w terminalu
  Serial.print("Aktualna data i czas: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.print(day);
  Serial.print(" - ");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.println(second);

  // opóźnienie jednej sekundy
  delay(1000);
}
```

Wyjście będzie wyglądać następująco:

```markdown
Aktualna data i czas: 2018/1/1 - 12:30:1
```

## Deep Dive

Funkcja "time" jest częścią biblioteki "Time.h", która jest dostarczana z Arduino IDE. Biblioteka ta definiuje funkcje do manipulacji czasem i datą. Funkcja "setTime" jest wykorzystywana do ustawiania bieżącego czasu i daty, podczas gdy funkcje "year", "month" itd. służą do pobierania odpowiednich części daty i czasu.

Pamiętaj, że funkcja "time" zwraca między innymi datę w formacie ponad 32 bitów, więc jeśli chcesz wyświetlić wszystkie elementy daty, musisz użyć odpowiednich typów zmiennych, np. "int" lub "long".

## Zobacz też

- [Dokumentacja Time.h](https://www.arduino.cc/en/reference/time)
- [Przykładowy projekt z wykorzystaniem Time.h](https://www.arduino.cc/en/Tutorial/BuiltInExamples/TimeSerial)