---
title:                "Interpolering av en sträng"
html_title:           "Arduino: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar är sättet att infoga variabler direkt i strängar. Programmerare använder det för att göra koden mer läsbar och minska skrivfel.

##Hur gör man:
Här är ett exempel på hur du interpolerar en sträng i Arduino:

```Arduino
int ålder = 25;
String namn = "Olle";

Serial.begin(9600);
Serial.println("Hej, jag heter " + namn + " och jag är " + ålder + " år gammal.");
```
Output:
```
Hej, jag heter Olle och jag är 25 år gammal.
```

## Djup dykning
Historiskt sett, innan stränginterpolering introducerades, använde programmerare konkatenering för att lägga in variabler i strängar, vilket kunde vara rörigt. Ett alternativ till stränginterpolering kan vara användning av sprintf-funktionen, men det kan leda till mer komplex kod. Stränginterpolering görs genom att lägga till variablets värde direkt i strängen vid runtime.

## Se också
1. [Arduino String Object](https://www.arduino.cc/reference/tr/language/variables/data-types/stringobject/)
2. [Arduino Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
3. [String Interpolation In C++](https://en.cppreference.com/w/cpp/language/string_literal)
4. [The Efficient Interpolation of Strings in Modern C++](https://levelup.gitconnected.com/the-efficient-interpolation-of-strings-in-modern-c-b542b48f01f3)