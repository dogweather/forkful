---
date: 2024-01-20 17:51:50.975147-07:00
description: "Hur g\xF6r man: Seriell utskrift \xE4r ingen nyhet. Elektronikutvecklare\
  \ har anv\xE4nt n\xE5gon form av debugutskrift sedan tidigt 1900-tal f\xF6r att\
  \ fels\xF6ka sina\u2026"
lastmod: '2024-04-05T22:50:52.479885-06:00'
model: gpt-4-1106-preview
summary: "Seriell utskrift \xE4r ingen nyhet."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Hur gör man:
```Arduino
void setup() {
  Serial.begin(9600);  // Startar seriekommunikation
}

void loop() {
  Serial.println("Hej från din Arduino!");  // Skriver ut ett meddelande
  delay(1000);  // Väntar en sekund
}
```
Exempelutskrift:
```
Hej från din Arduino!
Hej från din Arduino!
...
```

## Fördjupning
Seriell utskrift är ingen nyhet. Elektronikutvecklare har använt någon form av debugutskrift sedan tidigt 1900-tal för att felsöka sina apparater. Alternativ till seriel kommunikation inkluderar användning av lysdioder eller ljudsignaler men detta ger mindre information. Implementeringsdetaljer kring serial utskrift i Arduino inkluderar inställning av baudhastighet och användning av rätt COM-port.

## Se även
- [Arduino's Serial.begin()](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
- [Adafruit's Guide to Arduino Serial Debugging](https://learn.adafruit.com/adafruit-arduino-lesson-5-the-serial-monitor/overview)
