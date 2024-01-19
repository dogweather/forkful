---
title:                "Starter et nytt prosjekt"
html_title:           "Arduino: Starter et nytt prosjekt"
simple_title:         "Starter et nytt prosjekt"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Å Starte et Nytt Prosjekt i Arduino: En Praktisk Guide

## Hva & Hvorfor?

Å starte et nytt prosjekt er så enkelt som å åpne en ny, tom skisse i Arduino IDE. Programmerere gjør dette for å lage egendefinerte programmer.

## Hvordan:

La oss starte med en enkel "blinkende LED" kode. 

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```
Dette programmet vil få den innebygde LEDen til å blinke hvert sekund.

## Deep Dive

Arduino ble lansert i 2005 som et brukervennlig alternativ for microcontroller-bruk. Alternativene til Arduino inkluderer Raspberry Pi for mer komplekse prosjekter, og Micro:bit for utdanningsformål.

Når du starter et nytt prosjekt i Arduino IDE, lages det en tom skisse. En Arduino 'skisse' er rett og slett programmet du laster opp til Arduino-brettet. Den inneholder to primære deler:  'setup()' og 'loop()'.

## Se Også

Sjekk ut disse nyttige kildene for mer informasjon:

1. Arduino's offisielle side: https://www.arduino.cc
2. Online Arduino kurs: https://www.coursera.org/courses?query=arduino
3. For mer komplekse prosjekter, se på: https://www.raspberrypi.org
4. For utdanningsformål, sjekk ut: https://microbit.org