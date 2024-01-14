---
title:                "Arduino: Söka och ersätta text"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med Arduino-programmering kan det ibland vara svårt att hålla reda på all kod och text som skrivs. För att göra det mer effektivt och strukturerat, kan man använda sökning och ersättning för att hitta och ändra text i sitt program. Detta hjälper till att spara tid och undvika misstag.

## Hur man gör

För att söka och ersätta text i Arduino-program kan man använda funktionerna ```find()``` och ```replace()```. 

Exempel:

```Arduino
String text = "Hej, världen!";
text.replace("Hej", "Hello");
Serial.println(text);
```

Output:

```Arduino
Hello, världen!
```

Man kan även använda sökfunktionen ```indexOf()``` för att hitta positionen för en viss text i en sträng, och sedan ersätta den genom att använda ```setCharAt()```-funktionen.

Exempel:

```Arduino
String text = "Hej, världen!";
int position = text.indexOf("världen");
text.setCharAt(position, 'W');
Serial.println(text);
```

Output:

```Arduino
Hej, World!
```

Det är även möjligt att använda reguljära uttryck för mer avancerad sökning och ersättning av text.

## Djupdykning

För att lära sig mer om sökning och ersättning av text i Arduino-program, kan det vara användbart att lära sig om olika metoder för stränghantering i Arduino, såsom ```substring()```, ```concat()``` och ```compareTo()```. Det kan även vara användbart att använda en texteditor med inbyggd sökfunktion för att lättare navigera och ändra i sin kod.

## Se även

Här är några länkar som kan vara användbara för att lära sig mer om sökning och ersättning av text i Arduino-program:

- [String-biblioteket i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Tutorial om stränghantering i Arduino](https://startingelectronics.org/software/arduino/learn-to-program-course/06-strings-in-arduino/)
- [Reguljära uttryck i Arduino](https://www.arduino.cc/reference/en/language/functions/advanced-io/regular-expressions/)
- [En avancerad guide till stränghantering i Arduino](https://www.best-microcontroller-projects.com/arduino-string.html)