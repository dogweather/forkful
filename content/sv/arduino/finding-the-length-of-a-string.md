---
title:                "Att hitta längden på en sträng"
html_title:           "Arduino: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är viktigt när man arbetar med att hantera textdata. Det kan till exempel vara användbart när man vill kontrollera inmatade värden eller bearbeta textsträngar på ett effektivt sätt. Genom att lära sig hur man hittar längden på en sträng kan du utöka dina programmeringsfärdigheter och göra din kod mer mångsidig.

## Hur man gör
Att hitta längden på en sträng är en relativt enkel uppgift i Arduino-programmering. Först behöver vi inkludera biblioteket "String.h" i vår kod. Sedan kan vi använda metoden "length()" för att hitta längden på en specifik sträng. Nedan är ett exempel på hur vi kan göra detta:
```Arduino
#include <String.h>

// Deklarera en strängvariabel
String text = "Hej, världen!";

// Använda metoden "length()" för att hitta längden på strängen
int length = text.length();

// Skriv ut längden på seriel monitor
Serial.println(length);
```
Detta kommer att resultera i outputen "13" eftersom det är antalet tecken i vår sträng, inklusive mellanslag.

## Djupgående
När vi använder metoden "length()" för att hitta längden på en sträng, räknas även mellanslag och specialtecken som en del av längden. Om du vill hitta längden utan mellanslag kan du istället använda metoden "trim().length()". Detta tar bort mellanslag och andra "whitespace" tecken innan den räknar längden.

Det är också viktigt att notera att längden som returneras av "length()" är av typen "int", vilket innebär att den bara kan hantera max 32-bitars värden. Om du behöver hitta längden på en sträng som är längre än detta, kan du använda metoden "size()" istället, som hanterar 64-bitars värden.

## Se även
- [Officiell dokumentation för String.h biblioteket på Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Tutorial för att hantera textsträngar i Arduino](https://maker.pro/arduino/tutorial/arduino-tutorial-how-to-work-with-strings-in-arduino)
- [Exempelkod för att hitta längden på en sträng utan att använda metoden "length()"](https://www.arduino.cc/reference/en/language/string/functions/size/)