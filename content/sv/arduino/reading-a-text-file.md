---
title:    "Arduino: Läsa en textfil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en viktig del av många programmeringsprojekt. Det kan hjälpa dig att spara och hämta data som behövs för dina Arduino-program. Här lär vi dig varför läsa en textfil är viktigt och hur du gör det på bästa sätt.

## Hur man gör

För att läsa en textfil i Arduino behöver du först öppna filen. Detta görs med funktionen `open()` som tar två argument - namnet på filen och vilken läge den ska öppnas i. Om filen inte finns kommer den att skapas automatiskt. Sedan kan du använda funktionen `read()` för att läsa in varje rad i filen och spara den i en variabel. Här är ett exempel:

```Arduino
File textFile = SD.open("data.txt", FILE_READ);

String line = "";

while (textFile.available()) {
  line = textFile.readStringUntil('\n');
  Serial.println(line);
}

textFile.close();
```

Koden ovan öppnar filen "data.txt" för läsning, läser in varje rad och skriver ut den på seriell monitor. Notera att filen behöver stå i ett SD-kort för att kunna läsas. Om du vill spara datan som lästs i en variabel istället för att skriva ut den, kan du använda `textFile.readString()` istället.

## Djupdykning

Det finns flera saker att tänka på när man läser en textfil i Arduino. Först och främst behöver du ha tillgång till en SD-kortläsare för att kunna läsa filer på SD-kortet. Dessutom kan du använda olika metoder för att läsa filen, beroende på vilken typ av datan är sparad som. Till exempel kan du använda `parseJASON()` om datan är i JSON-format eller `parseInt()` om det är numerisk datan.

## Se även

För mer information och exempel på hur man läser en textfil i Arduino, se följande länkar:

- [Läsa en textfil på SD-kortet](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadFile)
- [Serial.read()](https://www.arduino.cc/reference/en/language/functions/communication/serial/read/)
- [Arduino SD library](https://www.arduino.cc/en/Reference/SD)