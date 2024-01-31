---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-19
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är processen att verifiera en mapps närvaro i filsystemet. Programmerare gör detta för att undvika fel vid filoperationer, som att läsa från eller skriva till icke-existerande mappar.

## Hur gör man:
Använd `SD` biblioteket för att interagera med filsystemet. För att kontrollera om en katalog finns, prova följande kod:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("SD card initialization failed");
    return;
  }
  
  File root = SD.open("/");
  if (root.isDirectory()) {
    File dir = root.openNextFile();
    while (dir) {
      if (dir.isDirectory()) {
        Serial.print("Directory Exists: ");
        Serial.println(dir.name());
        // Gör mer kod här om katalogen finns
      }
      dir = root.openNextFile();
    }
  }
}

void loop() {
  // Inget behövs i loopen
}
```

Sample output:
```
Directory Exists: MYDIR/
```

## Fördjupning
Att kontrollera om en katalog finns på ett SD-kort med Arduino började när SD-biblioteket lanserades, vilket möjliggjorde enkel tillgång till filsystemet. Det finns alternativ som `SDFat` biblioteket som också kan hantera filsystemoperationer. Implementationen är ganska rätfram: `SD.open()` öppnar katalogen, `isDirectory()` kontrollerar om den är en katalog, och `openNextFile()` itererar genom filsystemets objekt.

## Se även
- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [SDFat Library](https://github.com/greiman/SdFat)
