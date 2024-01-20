---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skapa en tillfällig fil i programmering används för att lagra data temporärt under programmets körning. Det hjälper till att hantera och minska minnesanvändningen, särskilt i stora applikationer. 

## Hur göra:

Arduino stöder inte direkt skapandet av temporära filer på grund av hårdvarubegränsningar, men vi kan skapa liknande funktionalitet med EEPROM. Här är ett exempell:

```Arduino
#include <EEPROM.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  // Läser data från en adress (10 i detta fall)
  uint8_t value = EEPROM.read(10);
  Serial.println(value, HEX);

  // Skriv data till samma adress
  // Vi kommer att skriva en sträng "A" för att demonstrera
  EEPROM.write(10, 'A');

  // Läs tillbaka den skrivna datan
  value = EEPROM.read(10);
  Serial.println(value, HEX);

  delay(1000);
}
```
Exempelutmatning:

```Arduino
0
41
```

## Djupdykning

Historiskt sett kunde datorer inte skapa tillfälliga filer på samma sätt som modernt bestående minne gör. Raderbara programmerbara läsbara minnen (EEPROM) introducerades för att lagra data mellan strömbrytningar.

Alternativ för att skapa en temporär fil är variabler och datastrukturer. Men om programmet behöver sätta ett tak för minnesanvändningen, är en temporär fil ett bättre alternativ. 

Implementeringen av en temporär fil i Arduino, särskilt på EEPROM, bör göras noggrant eftersom EEPROM har en skrivgräns på omkring 100,000 skrivningar. 

## Se också

För vidare studier om ämnet, kolla in dessa källor:

- [Arduino EEPROM Dokumentation](https://www.arduino.cc/en/Reference/EEPROM)
- [EEPROM tålighet](https://www.digikey.com/eewiki/pages/viewpage.action?pageId=4096093) 
- [Alternativ till EEPROM](https://www.avrfreaks.net/forum/avr-eeprom-alternatives)