---
title:                "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil kan vara en användbar del av din Arduino programmeringsupplevelse. Det ger dig möjlighet att spara data och information på ett enkelt sätt, vilket kan vara användbart för projekt som kräver lagring av variabler eller sensoravläsningar.

## Hur man gör

För att skriva en textfil på din Arduino, använd "File" biblioteket. Börja med att inkludera biblioteket i din kod:

```Arduino
#include <SPI.h>
#include <SD.h>
```

Därefter måste du initialisera SD-kortet och öppna en textfil på SD-kortet:

```Arduino
File textfil = SD.open("min_filstig.txt", FILE_WRITE);
```

Nu kan du skriva i textfilen med funktionen "println()":

```Arduino
textfil.println("Det här är en ny rad i min textfil.");
```

När du är klar med att skriva i filen, ska du stänga den med "close()"-funktionen:

```Arduino
textfil.close();
```

## Djupdykning

För att skriva en textfil med mer avancerade funktioner, titta på "File" bibliotekets dokumentation. Du kan till exempel kontrollera om en fil redan finns på SD-kortet med "exists()" funktionen eller skapa en ny mapp för din textfil med "mkdir()" funktionen.

Kom också ihåg att filnamnet och sökvägen måste anpassas efter dina behov. Du kan använda variabler i filnamnet vilket gör det möjligt att dynamiskt skapa olika filer baserat på olika värden.

## Se även

- [Filbibliotekets dokumentation](https://www.arduino.cc/en/Reference/SD)
- [SD-kortet anslutet till Arduino](https://www.arduino.cc/en/Guide/ArduinoMKRSd)
- [Markbrott](https://www.markdownguide.org/)