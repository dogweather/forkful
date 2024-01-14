---
title:                "Arduino: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego warto wyświetlać debug output? 
W programowaniu Arduino debug output jest niezbędnym narzędziem do monitorowania działania naszego kodu. Dzięki wyświetlaniu odpowiednich informacji na ekranie lub w terminalu możemy śledzić przebieg programu i w razie potrzeby wprowadzać poprawki, co ułatwia nam tworzenie optymalnych projektów.

## Jak wyświetlać debug output na Arduino?
Wyświetlenie debug output jest bardzo proste i wymaga użycia funkcji ```Serial.print()``` lub ```Serial.println()```. Przykładowy kod wyglądałby następująco:

```Arduino 
int sensorValue = analogRead(A0); // odczytanie wartości z czujnika
Serial.println("Odczytano wartość: "); // wyświetlenie tekstu na ekranie
Serial.println(sensorValue); // wyświetlenie wartości odczytanej z czujnika
```

Po przesłaniu tego kodu do płytki Arduino w terminalu pojawi się informacja o odczytanej wartości z czujnika, co pozwala nam na bieżąco monitorować działanie naszego programu.

## Głębsza analiza wyświetlania debug output
Istnieją różne rodzaje funkcji do wyświetlania debug output, między innymi ```Serial.print()```, ```Serial.println()```, ```Serial.printf()``` czy ```Serial.write()```. Każda z nich ma swoje specyficzne zastosowanie, dlatego ważne jest, aby zapoznać się z nimi dokładniej. Istotne jest również ustawienie odpowiedniej prędkości transmisji, aby uniknąć błędów i zapewnienia płynnego działania wyświetlania danych.

## Zobacz również
- Dokumentacja funkcji ```Serial.print()``` i ```Serial.println()```: https://www.arduino.cc/reference/tr/language/functions/communication/serial/print/
- Przewodnik po debugowaniu na Arduino: https://www.arduino.cc/en/Guide/ArduinoConsoleDebugger
- Tutoriale dotyczące wyświetlania debug output na różnych platformach: https://learn.sparkfun.com/tutorials/electronics-projects-in-scratch-july-24-2020/designing-electronics-projects-in-scratch/