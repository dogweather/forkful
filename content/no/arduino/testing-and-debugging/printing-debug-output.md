---
date: 2024-01-20 17:51:54.266485-07:00
description: "Slik gj\xF8r du: F\xF8r \"Serial\" ble standard, var feils\xF8king vanskeligere,\
  \ og lysdioder eller fysiske instrumenter ble brukt for \xE5 indikere status.\u2026"
lastmod: '2024-04-05T22:50:55.066486-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r \"Serial\" ble standard, var feils\xF8king vanskeligere, og lysdioder\
  \ eller fysiske instrumenter ble brukt for \xE5 indikere status."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

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
