---
title:                "Utvinna substrängar"
html_title:           "Arduino: Utvinna substrängar"
simple_title:         "Utvinna substrängar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
En viktig del av programmering är att kunna hantera textsträngar på ett effektivt sätt. Genom att extrahera delar av en textsträng kan du bearbeta och manipulera den på ett mer precist sätt. Detta är speciellt användbart när du arbetar med användarinput eller sensorvärden.

## Hur man gör
Extrahera en del av en textsträng genom att använda **substring** funktionen i Arduino. Här är ett exempel på hur man kan använda funktionen för att ta ut en del av en textsträng med hjälp av indexnumren för start- och slutpositionen:
```Arduino 
String text = "Hej alla Arduino entusiaster!";
String substring = text.substring(4, 8);

Serial.println(substring); // Output: "alla"
```

Du kan också använda **indexOf** funktionen för att hitta indexnumret för en specifik symbol eller bokstav i textsträngen. Sedan kan du använda **substring** funktionen för att extrahera texten från detta index och framåt. Till exempel:
```Arduino 
String text = "Jag älskar att koda med Arduino!";
int index = text.indexOf("koda");
String substring = text.substring(index);

Serial.println(substring); // Output: "koda med Arduino!"
```

## Djupdykning
För att bättre förstå hur substring funktionen fungerar är det viktigt att förstå det binära representationsformatet för text i en dator. Text är representerad som en serie av binära siffror, där varje tecken har en specifik siffra som representerar det i en ASCII-tabell. När du använder substring funktionen tar den dessa binära värden och konverterar dem till en förståelig textsträng baserat på teckenkoderna.

Det är också värt att notera att Arduino har en begränsning för storleken på en textsträng. Det är viktigt att hålla detta i åtanke när du använder substring funktionen för att undvika minnesöverskridning.

## Se även
För mer information om hur du hanterar textsträngar i Arduino, läs gärna följande artiklar:
- [Arduino Reference - substring](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino Reference - indexOf](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)