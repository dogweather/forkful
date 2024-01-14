---
title:    "Arduino: Skriva en textfil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att kunna skriva en textfil är en nyttig färdighet för alla som arbetar med Arduino-programmering. Det ger dig möjlighet att spara data och läsa från en textfil, vilket kan vara användbart för att lagra sensordata eller konfigurationsinställningar.

## Hur man gör

För att skriva en textfil med Arduino, används funktionen ```File.println()```. Detta tillåter dig att skriva en rad av text till en angiven fil. Nedan följer ett exempel som öppnar en fil, skriver en rad av text och stänger filen:

```Arduino
File myFile = SD.open("data.txt", FILE_WRITE); //Öppnar filen "data.txt" för skrivning på SD-kortet
myFile.println("Det här är en textfil"); //Skriv en rad med text till filen
myFile.close(); //Stänger filen för att spara ändringar
```

När du vill lägga till mer text till filen, öppna den igen och använd funktionen ```File.println()``` enligt ovan.

Notera att du först behöver inkludera SD-biblioteket i början av ditt program för att kunna använda filskrivningsfunktionerna. Detta görs med följande kod:

```Arduino
#include <SD.h>
```

## Fördjupning

När du skriver en textfil är det viktigt att ha koll på filens karaktärskodning. Arduino använder vanligtvis UTF-8 kodning för textfiler, vilket är ett universellt format som stöds av de flesta textredigerare.

Det är också viktigt att tänka på filnamnets längd och giltiga tecken. SD-biblioteket har vissa begränsningar när det gäller längden och tecknen som kan användas i filnamn, så se till att kontrollera detta innan du namnger din fil.

## Se även

- [SD-bibliotekets dokumentation](https://www.arduino.cc/en/reference/SD)
- [Filskrivning i Arduino](https://www.arduino.cc/en/Tutorial/Files) 
- [Karaktärskodning](https://en.wikipedia.org/wiki/Character_encoding)