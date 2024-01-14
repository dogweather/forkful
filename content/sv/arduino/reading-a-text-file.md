---
title:    "Arduino: Att läsa en textfil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Varför
Det kan vara viktigt att kunna läsa en textfil i Arduino-programmering, speciellt om du behöver lagra eller hämta data från en extern källa. Detta kan vara användbart för att utöka funktionaliteten hos dina projekt och skapa mer interaktiva och dynamiska lösningar.

# Hur du gör det
För att läsa en textfil i Arduino-programmering behöver du använda File-läsare-biblioteket. Detta bibliotek ger dig möjlighet att öppna och läsa en textfil från ett SD-kort eller ett annat extern lagringsmedium.

Här är ett exempel på kod som visar hur du kan läsa en textfil från ett SD-kort och skriva ut dess innehåll i seriell monitor:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
    Serial.begin(9600); // starta seriell kommunikation
    SD.begin(4); // initialisera SD-kortpinne
    File textFil = SD.open("textfil.txt"); // öppna textfilen
    while (textFil.available()) { // loopa igenom filen tills all text har lästs
        char text = textFil.read(); // läs en tecken från filen
        Serial.print(text); // skriv ut tecknet i seriell monitor
    }
    textFil.close(); // stäng filen när allting har lästs
}

void loop() {
    // din kod här
}
```

När du kör denna kod kommer du att se innehållet i din textfil skrivas ut i seriell monitor. Du kan också anpassa koden för att göra mer avancerade operationer med texten, till exempel spara den i en variabel eller utföra beräkningar på den.

# Djupdykning
För att förstå mer om hur File-läsare-biblioteket fungerar kan du läsa dokumentationen på Arduino:s hemsida eller titta på källkoden för biblioteket. Du kanske också vill lära dig mer om hur man hanterar textfiler i allmänhet, till exempel hur man skriver till en textfil eller lägger till data i en befintlig fil.

# Se även
- [File-läsare-bibliotekets dokumentation](https://www.arduino.cc/en/Reference/SD)
- [Källkod för File-läsare-biblioteket](https://github.com/arduino-libraries/SD)
- [Tips för att hantera textfiler i Arduino-projekt](https://learn.sparkfun.com/tutorials/how-to-work-with-text-files/all)