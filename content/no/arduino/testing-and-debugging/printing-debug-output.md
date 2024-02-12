---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/arduino/printing-debug-output.md
date:                  2024-01-20T17:51:54.266485-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive ut feilsøkingsdata ("debug output") handler om å vise programdata og -tilstander til skjermen for å forstå hva koden faktisk gjør. Programmerere gjør dette for å finne og rette feil effektivt.

## Slik gjør du:
```Arduino
void setup() {
  Serial.begin(9600); // Starter seriell kommunikasjon
}

void loop() {
  Serial.println("Hei, verden!"); // Skriver ut til seriell monitor
  delay(1000); // Venter ett sekund før neste utskrift
}
```
Eksempel på output:
```
Hei, verden!
Hei, verden!
Hei, verden!
...
```

## Dypdykk
Før "Serial" ble standard, var feilsøking vanskeligere, og lysdioder eller fysiske instrumenter ble brukt for å indikere status. Alternativer til `Serial` inkluderer LCD-skjermer eller nettverksprotokoller som MQTT for ekstern feilsøking. Når du bruker `Serial`, sendes data gjennom UART (Universal Asynchronous Receiver Transmitter) på Arduino-kortet. Overføringshastigheten (baudrate) må være lik på begge ender.

## Se Også
Arduino sin offisielle "Serial" dokumentasjon:
[https://www.arduino.cc/reference/en/language/functions/communication/serial/](https://www.arduino.cc/reference/en/language/functions/communication/serial/)

Forståelse av UART:
[https://learn.sparkfun.com/tutorials/serial-communication](https://learn.sparkfun.com/tutorials/serial-communication)

Feilsøking ved bruk av MQTT:
[https://randomnerdtutorials.com/what-is-mqtt-and-how-it-works/](https://randomnerdtutorials.com/what-is-mqtt-and-how-it-works/)
