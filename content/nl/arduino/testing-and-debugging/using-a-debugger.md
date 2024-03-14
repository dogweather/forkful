---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:29.337271-07:00
description: "Een debugger is een tool die je helpt om bugs in je code te verpletteren\
  \ door je de mogelijkheid te geven om te pauzeren, rond te kijken en te ontdekken\u2026"
lastmod: '2024-03-13T22:44:51.076403-06:00'
model: gpt-4-0125-preview
summary: "Een debugger is een tool die je helpt om bugs in je code te verpletteren\
  \ door je de mogelijkheid te geven om te pauzeren, rond te kijken en te ontdekken\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?

Een debugger is een tool die je helpt om bugs in je code te verpletteren door je de mogelijkheid te geven om te pauzeren, rond te kijken en te ontdekken wat er echt gaande is onder de motorkap. Programmeurs gebruiken debuggers om stap voor stap door hun code te lopen, variabelen te inspecteren en te begrijpen waar dingen mogelijk fout gaan.

## Hoe te:

Met de Arduino IDE kun je Serial prints gebruiken om te debuggen, maar het is een beetje alsof je een zaklamp gebruikt om een grot te verkennen. Voor echt debuggen wil je misschien je spel verbeteren met iets zoals de Atmel-ICE debugger die integreert met de Arduino-omgeving. Hier is een voorproefje van pseudo-debuggen met Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorwaarde = analogRead(A0);
  Serial.print("Sensorwaarde: ");
  Serial.println(sensorwaarde);
  // Stel je voor dat je hier 512 verwacht, maar 0 krijgt.
  // Tijd om de sensorverbinding te inspecteren
  delay(1000); // Wacht een seconde voor je opnieuw leest
}
```
Voer dit uit met de Serial Monitor open en je zult zien wat je sensor in real-time uitspuwt.

## Diepe Duik

Voor debuggers was het de wereld van print statements - je kon alleen raden wat er gebeurde door alles uit te printen. Debuggen met prints is nog steeds gebruikelijk, vooral in eenvoudigere omgevingen of op beperkte hardware zoals de Arduino.

Alternatieven voor in-circuit emulators zoals Atmel-ICE omvatten software-debugtools zoals `avr-gdb`. Je kunt het koppelen met `avarice` om een brug te vormen tussen GDB en je hardware, wat super handig is voor geavanceerder debuggen direct op de chip.

Met een debugger kun je breakpoints instellen om de uitvoering op bepaalde punten te stoppen. Je kunt regel voor regel door je code lopen, geheugen, registers en variabelen inspecteren. Dit stelt je in staat om problemen te pinpointen in plaats van schoten in het duister te nemen. Wanneer je een debugger implementeert, zorg ervoor dat je omgeving correct is ingesteld - niet overeenkomende versies of slecht geconfigureerde tools kunnen leiden tot frustratie.

## Zie Ook

Klaar om dieper te gaan? Duik in deze:
- De Arduino debugging gids op [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- De AVR Libc referentiehandleiding voor het instellen van avr-gdb: [AVR Libc Homepagina](http://www.nongnu.org/avr-libc/)
