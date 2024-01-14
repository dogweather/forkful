---
title:                "Arduino: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

Arduino Programmering: Att Läsa Textfiler

## Varför

Att läsa och hantera textfiler är en viktig del av Arduino programmering. Genom att kunna läsa textfiler kan du både få information från externa källor och spara data i en strukturerad form. Det är också ett användbart sätt att dela information mellan olika enheter.

## Hur man gör det

För att läsa en textfil i Arduino, behöver du först ansluta en SD-kortmodul till din Arduino. Ladda sedan ned SD-biblioteket från Arduino IDE och importera det i ditt program. För att öppna och läsa en textfil använder du funktionen "open()" följt av filens namn och läget "r" för att läsa filen.

```Arduino
#include <SD.h>
File textFile;

void setup() {
  // Anslut SD-kortmodulen till Arduino
  // Importera SD-biblioteket
  // Öppna textfilen för läsning
  textFile = SD.open("textfil.txt", FILE_READ);
}
```

Efter att filen är öppnad kan du läsa innehållet rad för rad med funktionen "readStringUntil()". Denna funktion tar ett tecken som argument och returnerar en sträng med allt innehåll tills det tecknet är nått. Du kan också använda funktionen "available()" för att kontrollera om det finns mer innehåll i filen.

```Arduino
// Läser en rad från filen och sparar den i en strängvariabel
String line = textFile.readStringUntil('\n');
// Kontrollerar om det finns mer innehåll i filen
if(textFile.available()){
  // Fortsätter att läsa och spara rader till strängen tills slutet av filen är nått
  line += textFile.readStringUntil('\n');
}
```

När du är klar med läsningen är det viktigt att stänga filen genom att använda funktionen "close()".

```Arduino
// Stänger filen när läsningen är klar
textFile.close();
```

## Djupdykning

Det finns flera funktioner i "SD"-biblioteket för att hantera textfiler. Du kan till exempel använda funktionen "seek()" för att gå till en specifik del av filen och läsa innehåll från den punkten. Du kan också skriva till en textfil med funktionen "write()" eller ta bort en befintlig fil med funktionen "remove()".

Att läsa textfiler i Arduino är ett användbart sätt att integrera extern information i dina projekt, som till exempel sensorvärden eller konfigurationsdata. Det ger också möjlighet att spara och dela data mellan enheter. Du kan läsa mer om alla funktioner och möjligheter med SD-biblioteket på Arduino's hemsida eller i dokumentationen.

## Se även

- [Arduino's hemsida](https://www.arduino.cc/)
- [SD-biblioteket dokumentation](https://www.arduino.cc/en/Reference/SD)
- [GitHub - SdFat](https://github.com/greiman/SdFat)