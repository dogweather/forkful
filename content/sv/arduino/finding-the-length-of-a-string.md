---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att hitta längden på en sträng innebär att ta reda på antalet tecken i strängen. Programmerare gör detta för att bearbeta, manipulera eller förstå data mer effektivt.

## Hur man:

Använd `strlen()` funktionen för att hitta strängens längd. Exempelvis, deklarera först strängvariabeln och använd sedan strlen funktionen.

```Arduino
char texting[] = "Hej världen";
unsigned int strLength;
strLength = strlen(texting);
Serial.println(strLength);
```

Output:

```
12
```

## Djupdykning

Historiskt sett har metoden att hitta strängens längd funnits länge i programmeringsspråk, och är en kärnfunktion i många typer av textbearbetning.

Det finns även alternativ till `strlen()`. `Strnlen()` kan användas för att hitta längden på en sträng, men med en bestämd maximal längd.

En detalj att notera är att `strlen()` returnerar antalet tecken innan upptäckt av den första nollan ('\0'). 

## Se även

[Arduino Reference - strlen](https://www.arduino.cc/reference/en/language/functions/strings/strlen/)
[Arduino Guide - String Manipulation](https://startingelectronics.org/software/arduino/arduino-IDE/arduino-C++/strings/)