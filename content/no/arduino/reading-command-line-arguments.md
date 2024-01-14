---
title:                "Arduino: Lese kommandolinje argumenter"
simple_title:         "Lese kommandolinje argumenter"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du ønsker å lære mer om hvordan du kan kommunisere med Arduino ved hjelp av kommandolinjeargumenter, er denne bloggen noe for deg.

## Hvordan
Kommandolinjeargumenter er en måte å gi instruksjoner og data til et program fra kommandolinjen. Dette kan være nyttig for Arduino-prosjekter som krever interaktiv kommunikasjon eller tilpasning av oppførsel.

En måte å lese kommandolinjeargumenter på i Arduino er ved å bruke funksjonen `serialEvent()`. Her er et eksempel på hvordan dette kan gjøres:

```Arduino
void setup() {
  Serial.begin(9600);
}

void serialEvent() {
  while (Serial.available()) {
    String data = Serial.readStringUntil('\n');
    Serial.println("Data mottatt: " + data);
  }
}
```

I dette eksempelet leser vi data fra serieporten (kommandolinjen) og skriver det ut til den serielle monitor. Du kan også bruke denne dataen til å styre andre funksjoner i programmet ditt.

## Dypdykk
En viktig ting å huske på når du leser kommandolinjeargumenter er å evaluere og validere dataen som mottas. Dette kan du gjøre ved å bruke innebygde funksjoner som `parseInt()` og `parseFloat()` for å konvertere dataen til tall, eller `equals()` for å sammenligne tekststrenger.

Du kan også bruke `strtok()` for å splitte dataen inn i forskjellige deler basert på et angitt separator-tegn.

## Se også
- [How to Use Command Line Arguments in Arduino Sketches](https://learn.sparkfun.com/tutorials/how-to-use-command-line-arguments-in-arduino-sketches/all)
- [Arduino Serial Event Handling](https://www.arduino.cc/reference/en/language/functions/communication/serial/serialevent/)
- [Communicating with Arduino Through the Command Line](https://thewindowsclub.com/communicating-with-arduino-through-the-command-line)