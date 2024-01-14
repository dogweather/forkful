---
title:                "Arduino: Skapa en temporär fil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapande av temporära filer kan vara användbart för att lagra data som endast behövs temporärt under körningen av ditt Arduino program. Det kan också hjälpa till att minska användningen av mikrokontrollerns minne.

## Hur man gör

För att skapa en temporär fil på ett Arduino kort, behöver du använda "File" biblioteket:

```Arduino
#include <SPI.h>
#include <SD.h>

File tempFile;

void setup() {
  // Mappar upp SD kortet
  SD.begin(SD_CS_PIN);
  
  // Öppnar en temporär fil för skrivning
  tempFile = SD.open("temp_file.txt", FILE_WRITE);
  
  // Skriver data till filen
  tempFile.print("Det här är en temporär fil.");
  
  // Stänger filen
  tempFile.close();
}

void loop() {
  // Ditt huvudsakliga program
}
```

Detta kodexempel visar hur man skapar en fil med namnet "temp_file.txt" på SD-kortet och skriver texten "Det här är en temporär fil" till den. När filen är stängd finns den tillgänglig för läsning och kan sedan tas bort när den inte längre behövs.

## Djupdykning

För att förstå varför och hur man skapar en temporär fil på ett Arduino kort, är det viktigt att förstå hur RAM och tillfällig lagring fungerar. RAM, eller Random Access Memory, är den del av mikrokontrollerns minne som används för att lagra programvariabler och data under körningen av programmet. Eftersom RAM ofta är begränsat på mikrokontrollrar, kan användningen av en temporär fil hjälpa till att frigöra dessa begränsningar.

När en temporär fil skapas på SD-kortet, kan data skrivas till den utan att påverka RAM. Detta betyder att mer data kan lagras utan att orsaka minnesproblem. Dessutom kan filen läsas och skrivas flera gånger utan att påverka tillgången till RAM.

För att ta bort en temporär fil från SD-kortet, kan "SD.remove()" funktionen användas:

```Arduino
SD.remove("temp_file.txt");
```

Detta kommer att ta bort den angivna filen från SD-kortet och frigöra utrymme för andra filer.

## Se även

- Arduino File Library: https://www.arduino.cc/en/Reference/SD
- Arduino SD Library: https://www.arduino.cc/en/Reference/SD
- SD Card tutorial: https://www.arduino.cc/en/Tutorial/SD


[In Swedish]
## Se även

- Arduino File Library: https://www.arduino.cc/en/Reference/SD
- Arduino SD Library: https://www.arduino.cc/en/Reference/SD
- SD Card tutorial: https://www.arduino.cc/en/Tutorial/SD