---
title:                "Porównywanie dwóch dat"
html_title:           "Arduino: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie kodu do porównywania dat może być przydatne w różnych projektach, od tworzenia kalendarzy po systemy monitorowania czasu. Porównywanie dat pomaga w ustalaniu szybkości zmian w danych i w podejmowaniu decyzji na podstawie czasu.

## Jak to zrobić

```Arduino
// Ustalenie pierwszej daty
int dzien1 = 12;
int miesiac1 = 6;
int rok1 = 2020;

// Ustalenie drugiej daty
int dzien2 = 15;
int miesiac2 = 6;
int rok2 = 2020;

// Porównanie dat
if (rok1 > rok2) {
  Serial.println("Pierwsza data jest późniejsza niż druga data.");
} else if (rok2 > rok1) {
  Serial.println("Druga data jest późniejsza niż pierwsza data.");
} else {
  if (miesiac1 > miesiac2) {
    Serial.println("Pierwsza data jest późniejsza niż druga data.");
  } else if (miesiac2 > miesiac1) {
    Serial.println("Druga data jest późniejsza niż pierwsza data.");
  } else {
    if (dzien1 > dzien2) {
      Serial.println("Pierwsza data jest późniejsza niż druga data.");
    } else if (dzien2 > dzien1) {
      Serial.println("Druga data jest późniejsza niż pierwsza data.");
    } else {
      Serial.println("Obie daty są takie same.");
    }
  }
}
```

Output w monitorze szeregowym (Serial Monitor):
```
Obie daty są takie same.
```

## Deep Dive

Istnieją różne sposoby na porównywanie dat w Arduino, w zależności od potrzeb i formatu dat. Można wykorzystać biblioteki, takie jak Time, która oferuje funkcje do wykonywania obliczeń na czasie i datach. Można też wykorzystać różne metody matematyczne, takie jak przeliczanie dat na liczby całkowite i porównywanie ich.

W przypadku porównywania dat pojawia się też kwestia czasu uniwersalnego. Arduino nie ma wbudowanej funkcji do obsługi czasu uniwersalnego, dlatego może być potrzebne dodanie dodatkowych komponentów, takich jak moduł RTC (Real-time clock).

## Zobacz też

- [Dokumentacja Arduino](https://www.arduino.cc/reference/pl/)
- [Biblioteka Time](https://www.arduino.cc/en/reference/time)
- [Porównywanie dat w Arduino tutorial](https://www.arduino.cc/en/Tutorial/CompareDates)