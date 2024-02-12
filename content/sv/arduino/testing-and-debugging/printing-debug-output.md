---
title:                "Skriva ut felsökningsdata"
aliases:
- /sv/arduino/printing-debug-output.md
date:                  2024-01-20T17:51:50.975147-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
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
