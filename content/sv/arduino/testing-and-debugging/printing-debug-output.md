---
date: 2024-01-20 17:51:50.975147-07:00
description: "Debugutskrift hj\xE4lper utvecklare att f\xF6rst\xE5 vad deras kod g\xF6\
  r genom att visa meddelanden i realtid. Programmerare g\xF6r detta f\xF6r att enkelt\
  \ hitta och\u2026"
lastmod: '2024-03-13T22:44:38.169733-06:00'
model: gpt-4-1106-preview
summary: "Debugutskrift hj\xE4lper utvecklare att f\xF6rst\xE5 vad deras kod g\xF6\
  r genom att visa meddelanden i realtid. Programmerare g\xF6r detta f\xF6r att enkelt\
  \ hitta och\u2026"
title: "Skriva ut fels\xF6kningsdata"
---

{{< edit_this_page >}}

## Vad & Varför?
Debugutskrift hjälper utvecklare att förstå vad deras kod gör genom att visa meddelanden i realtid. Programmerare gör detta för att enkelt hitta och åtgärda fel under utvecklingen.

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
