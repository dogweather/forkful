---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
simple_title:         "Pisanie do standardowego błędu"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wypisywanie do standardowego błędu (stderr) pozwala programom oddzielać normalne wyjście (stdout) od komunikatów o błędach. Programiści robią to, by łatwiej było im śledzić i przetwarzać błędy.

## Jak to zrobić:
Arduino nie ma dedykowanego stderr jak większość systemów operacyjnych, więc użyjemy Serial do symulacji.

```Arduino
void setup() {
  // Zacznij komunikację szeregową
  Serial.begin(9600);
}

void loop() {
  // Wysyłanie normalnych danych na standardowe wyjście
  Serial.println("Witaj, świecie!");

  // Symulacja stderr przez prefix błędu
  Serial.println("ERROR: Coś poszło nie tak!");
  
  // Czekaj 5 sekund
  delay(5000);
}
```

Wynik symulowanego stderr:
```
Witaj, świecie!
ERROR: Coś poszło nie tak!
```

## Deep Dive
Historia stderr sięga systemów Unix, gdzie oddzielono wyjście programu od informacji o błędach. W Arduino brakuje prawdziwego stderr, ale można wykorzystać Serial do tego celu. Alternatywą może być użycie dedykowanego pinu do sygnalizowania stanów błędu.

## Zobacz także
- [Arduino Reference: Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Wikipedia: Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
