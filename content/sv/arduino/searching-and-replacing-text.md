---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
I programmering, sökning och ersättning av text innebär att hitta en specifik sträng av tecken och ersätta den med en annan. Detta är viktigt för att modifiera och uppdatera data utan att behöva skriva om allt från början.

## Så gör du:
Arduino tillhandahåller `replace()`-metoden för att söka och ersätta text i en sträng. Kolla på följande kodexempel:

```Arduino
String str = "Hej världen!";
str.replace("Hej", "Hola");
Serial.println(str);  // Output: Hola världen!
```
I exemplet ovan söker vi strängen `Hej` och ersätter den med `Hola`. `println()`-funktionen skriver sedan ut den uppdaterade strängen.

## Djup Dykning
Historiskt sett, sökfunktioner var viktig för tidig datamanipulation. Innan `replace()` metoden, var sök och ersätt operationer tidskrävande och inbegripa komplicerade algoritmer.

Det finns alternativ till `replace()`-metoden. Du kan använda ett bibliotek som Regexp för mer komplexa matchnings- och ersättningshandlingar eller skriva din egen funktion för specifika användningsfall.

`replace()`-metoden fungerar genom att först hitta indexet för söksträngen i målsträngen. Om söksträngen hittas, tar den bort söksträngen och infogar ersättningssträngen på samma plats.

## Se även:
Letar du efter mer komplexa exempel och ytterligare information? Här är några länkar att kolla in:

- [Arduinos officiella referens för Stringreplace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Ett djupare dyk in i Arduino strängmanipulation](https://learn.adafruit.com/category/learn-arduino)
- [Arduino Strings tutorial på TutorialsPoint](https://www.tutorialspoint.com/arduino/arduino_strings.htm)