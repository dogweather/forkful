---
title:    "Arduino: Vergleich von zwei Daten"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten kann in der Arduino-Programmierung nützlich sein, um beispielsweise zu überprüfen, ob ein bestimmtes Ereignis in der Vergangenheit oder Zukunft liegt oder um Zeitintervalle zu berechnen.

# Wie geht man vor

In der Arduino-Programmierung können zwei Daten mit der Funktion `compareDates()` verglichen werden. Diese Funktion gibt entweder einen Wert kleiner als 0 zurück, wenn die erste Datei früher als die zweite ist, 0, wenn beide gleich sind, oder einen Wert größer als 0, wenn die erste Datei später ist als die zweite.

```Arduino 
#include <Time.h>

void setup() {
  // Datum und Zeit definieren
  tmElements_t date1 = {0, 13, 12, 30, 7, 2021};
  tmElements_t date2 = {0, 8, 12, 15, 6, 2021};

  int result = compareDates(date1, date2); // Vergleicht die Daten und gibt das Ergebnis zurück

  // Ausgabe des Ergebnisses
  if (result < 0) {
    Serial.println("Date 1 liegt vor Date 2.");
  } else if (result == 0) {
    Serial.println("Beide Daten sind gleich.");
  } else {
    Serial.println("Date 1 liegt nach Date 2.");
  }
}

void loop() {
  // Code hier eingeben
}
```

Output:
```
Date 1 liegt nach Date 2.
```

# Tiefere Einblicke

Bei der Vergleichsfunktion in der Time Library werden die Daten in Sekunden seit 1970 umgewandelt und dann verglichen. Dieser Wert ist bei beiden Dateien gleich, da sie jeweils den 30.12.2021 darstellen. Daher ist das Ergebnis größer als 0 und Date 1 liegt nach Date 2.

Es ist auch möglich, die Funktion `calculateTimeInterval()` zu verwenden, um die Differenz zwischen zwei Daten in Sekunden, Minuten, Stunden etc. zu berechnen. Diese Funktion nutzt ebenfalls die gespeicherten Werte der Daten in Sekunden.

# Siehe auch

- [Time Library Reference](https://www.arduino.cc/reference/en/libraries/time/) (Englisch)
- [Datums- und Zeit-Funktionen in Arduino](https://blog.halvske.de/datums-und-zeit-funktionen-in-arduino/) (Deutsch)