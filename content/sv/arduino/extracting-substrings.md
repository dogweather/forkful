---
title:                "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar (delsträngar) är ett användbart verktyg inom Arduino-programmering för att hantera och manipulera strängar. Genom att välja ut en del av en sträng kan du bearbeta en specifik del av data för att sedan använda den i ditt program.

## Hur

För att extrahera substrängar behöver du först använda funktionen `substring()`. Detta gör det möjligt att välja en del av en befintlig sträng genom att ange start- och slutpositionen för den önskade delen.

```Arduino
String str = "Hej! Jag heter Arduino.";
// Extrahera "Arduino" från strängen
String substr = str.substring(16, 23);
// Skriv ut substrängen "Arduino"
Serial.println(substr); // Output: "Arduino"
```

Du kan också använda variabler för att definiera start- och slutpositionerna för substrängen, vilket ger mer flexibilitet i ditt program.

```Arduino
// Definiera variabler för start- och slutposition
int startPos = 16;
int endPos = 23;
// Extrahera substrängen från variablerna
String substr = str.substring(startPos, endPos);
// Skriv ut substrängen "Arduino"
Serial.println(substr); // Output: "Arduino"
```

## Deep Dive

För att välja en del av en sträng måste du förstå hur teckenindexering fungerar. Varje tecken i en sträng har en unik plats eller position, som kallas index. Indexeringen börjar alltid från 0, vilket betyder att det första tecknet i en sträng har index 0, det andra tecknet har index 1 och så vidare.

Så om du vill extrahera en del av en sträng måste du veta start- och slutpositionen baserat på teckenindexeringen. Genom att använda funktionen `substring()` kan du sedan välja ut en del av strängen och använda den i ditt program.

## Se också

För mer information och exempel på hur du kan använda funktionen `substring()`, kolla in följande länkar:

- [Arduino Reference: substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutorial: Using Substrings in Arduino](https://www.allaboutcircuits.com/technical-articles/using-substrings-in-c-programming-with-arduino/)