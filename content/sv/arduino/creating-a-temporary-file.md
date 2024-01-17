---
title:                "Skapa en temporär fil"
html_title:           "Arduino: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil är ett vanligt sätt för programmerare att temporärt lagra data eller information som behövs under programkörningen. Det kan vara till nytta för att undvika att behöva skriva data på ett annat ställe eller för att enkelt ta bort denna data när den inte längre behövs.

## Hur gör man?
För att skapa en temporär fil i Arduino behöver vi först inkludera biblioteket "SD" som ger oss möjlighet att skriva till ett externs SD-kort. Sedan kan vi använda funktionen "open" från SD-biblioteket för att skapa en temporär fil med ett unikt namn. Här är ett exempel på hur detta kan se ut:

```
#include <SD.h>

File tempFile;
tempFile = SD.open("temp.txt", FILE_WRITE);
```

Den första raden inkluderar SD-biblioteket och den andra raden skapar en fil med namnet "temp.txt" som vi kan skriva till. Vi kan sedan använda kommandon som "tempFile.println" för att lägga till text eller data i den temporära filen.

## Djupdykning
Att skapa en temporär fil kan vara särskilt användbart när man behöver lagra data som är för stort för att sparas i Arduinos minne. Genom att använda ett extern SD-kort kan vi öka vår möjlighet att lagra större mängder data. Det finns också andra alternativ för att skapa temporära filer, som att använda en USB-enhet eller ett RAM-minne.

Det kan också vara värt att notera att det är viktigt att stänga och ta bort den temporära filen när den inte längre behövs för att undvika problem som kan uppstå om filen förblir öppen.

## Se även
Mer information om hur man använder SD-biblioteket för att skapa en temporär fil finns på Arduinos hemsida: https://www.arduino.cc/en/Reference/SD.