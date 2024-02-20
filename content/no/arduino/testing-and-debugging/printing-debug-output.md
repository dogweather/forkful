---
date: 2024-01-20 17:51:54.266485-07:00
description: "\xC5 skrive ut feils\xF8kingsdata (\"debug output\") handler om \xE5\
  \ vise programdata og -tilstander til skjermen for \xE5 forst\xE5 hva koden faktisk\
  \ gj\xF8r. Programmerere\u2026"
lastmod: 2024-02-19 22:05:00.331757
model: gpt-4-1106-preview
summary: "\xC5 skrive ut feils\xF8kingsdata (\"debug output\") handler om \xE5 vise\
  \ programdata og -tilstander til skjermen for \xE5 forst\xE5 hva koden faktisk gj\xF8\
  r. Programmerere\u2026"
title: "Skrive ut feils\xF8kingsdata"
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
