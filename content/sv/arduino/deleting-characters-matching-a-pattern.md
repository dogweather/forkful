---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att ta bort tecken som matchar ett mönster handlar om att leta efter och eliminera specifik kod och andra tecken från ditt Arduino-program. Programmerare gör det för att förbättra kodens effektivitet, minska minnesanvändningen och göra programmet mer läsbart.

## Hur man gör:
Här är exempel på hur du kan radera tecken som matchar ett mönster i Arduino:

```Arduino
String str = "HejVärlden123";
str.remove(str.indexOf('1'),3);
Serial.println(str);    // Skriver ut "HejVärlden"
```
Detta kodexempel tar bort alla tecken som börjar med '1' och de två tecknen som följer '1'.

## Djup Dykning
Detta är en teknik som går tillbaka till de tidiga dagarna av programmering. Ett alternativ till att ta bort tecken i en sträng är att ersätta dem. `str.replace('Hej', 'Hej Hej');` skulle ändra strängen till "Hej HejVärlden123". Du kan behöva manipulera strängar på detta sätt för att formatera data korrekt för olika I/O-enheter.

Värdet som passas till remove-funktionen är indexet för det datum som ska tas bort. Var noga med att kontrollera att indexet inte är -1 (som det skulle vara om datumen inte finns) eftersom detta skulle kunna radera hela strängen i vissa implementationer.

## Se även
För mer information om string-manipulation i Arduino, kontrollera följande källor:

1. [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [Manipulating Characters and Digging Deeper into ASCII in Arduino](https://create.arduino.cc/projecthub/electropeak/manipulating-characters-and-digging-deeper-into-ascii-in-arduino-fd6a45)
3. [Arduino String Tutorial: How to Split a String into Substrings](https://lastminuteengineers.com/arduino-string-tutorial/)
4. [Arduino Cookbook, Text Strings](https://www.oreilly.com/library/view/arduino-cookbook/9781449313876/ch04s07.html)