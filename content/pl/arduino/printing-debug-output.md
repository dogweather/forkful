---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Drukowanie komunikatów diagnostycznych (debug output) to metoda, która pozwala programistom śledzić wartości zmiennych i stan programu podczas jego działania. Dzięki temu, mogą oni łatwo lokalizować i rozwiązywać problemy, które pojawiają się w trakcie działania programu.

## Jak to zrobić:

Aby wydrukować komunikat diagnostyczny w Arduino, używamy funkcji `Serial.print()`. Poniżej znajduje się przykładowy kod i jego wyjście:

```Arduino
void setup() {
  Serial.begin(9600); // Inicjalizacja portu szeregowego
}

void loop() {
  int sensorValue = analogRead(A0); // Odczyt wartości z pinu A0
  Serial.print("Sensor Value: "); 
  Serial.println(sensorValue); // Drukowanie wartości z sensora
  delay(1000); // Czekaj sekundę 
}
```

Po uruchomieniu, otrzymasz poniższy wynik na podglądzie szeregowym:

```
Sensor Value: 455
Sensor Value: 460
Sensor Value: 465
...
```

## Głębsze spojrzenie:

W swojej najprostszej formie, drukowanie komunikatów diagnostycznych zostało wprowadzone w czasach, kiedy pamięć była cenna, a debuggery nie istniały. Programiści używali go, aby śledzić jak ich kod obchodził się z pamięcią i znajdywał błędy. 

Innym podejściem jest użycie narzędzi do debugowania (debuggers), które mogą dostarczyć o wiele więcej informacji na temat działania programu, ale są też bardziej skomplikowane do użycia.

W Arduino, drukowanie komunikatów diagnostycznych jest realizowane za pomocą interfejsu szeregowego, co oznacza, że są one transmitowane z płytki Arduino do komputera za pomocą tego samego kanału, którym jest przesyłany program.

## Zobacz także:

1. Dokumentacja Arduino na temat drukowania na porcie szeregowym: [Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
2. Poradnik na temat debugowania programów Arduino: [Debugging Arduino Code](https://www.arduino.cc/en/Tutorial/LibraryExamples/SoftwareSerialExample)
3. Tutorial na temat użycia debugowania szeregowego w Arduino: [Serial Debugging](https://learn.sparkfun.com/tutorials/serial-debugging)