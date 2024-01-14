---
title:                "Arduino: Porównywanie dwóch dat"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest istotną umiejętnością w programowaniu Arduino, ponieważ pozwala na stworzenie bardziej zaawansowanych, dynamicznych programów. Dzięki temu można bez trudu monitorować różne zdarzenia, takie jak zmiana czasu, lub akcja wywołana określoną datą. 

## Jak to zrobić

Aby porównywać daty w Arduino, używamy funkcji `if` oraz operatora logicznego `==` w celu porównania wartości. Na przykład, jeśli chcemy sprawdzić, czy dana data jest równa 1 stycznia, używamy następującego kodu:

```Arduino 
if (data == 0101) {
  Serial.println("Dzisiaj jest pierwszy stycznia!");
}
```

Jeśli chcesz wyświetlić bieżącą datę, można także użyć funkcji `millis()` oraz odpowiedniego formatowania:

```Arduino
unsigned long czas = millis();
String data = String(czas, DEC);
Serial.print("Aktualna data to: ");
Serial.println(data.substring(data.length() - 4, data.length()));
```

Powyższy kod używa funkcji `millis()` do pobrania aktualnego czasu w milisekundach, a następnie zamienia go na string i wypisuje tylko ostatnie 4 liczby, czyli rok.

## Głębszy zanurzenie

W przypadku bardziej złożonych porównań dat, można skorzystać z funkcji `time` oraz `timeStruct` w bibliotece Time. Pozwala to na tworzenie struktur zawierających informacje o dacie i czasie, co ułatwia porównywanie wartości. Przykładowy kod wykonujący to zadanie wyglądałby tak:

```Arduino
#include <Time.h>
time_t now = time(23, 59, 59, 31, 12, 2020);
timeStruct czas = breakTime(now);
if (czas.year == 2020) {
  Serial.println("Ta data jest w roku 2020!");
}
```

## Zobacz także

- Dokumentacja funkcji Time: https://www.arduino.cc/en/Reference/Time
- Przewodnik po porównywaniu danych w Arduino: https://www.arduino.cc/reference/en/language/functions/comparison-and-type-testing/