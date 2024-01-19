---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konkatenera strängar innebär att man kopplar ihop två eller flera teckensekvenser. Programmerare gör detta för att smidigt skapa en större textsträng från mindre delar.

## Hur fungerar det:

I Arduino skrivs kod för att konkatenera strängar så här:

```Arduino
String str1 = "Hej";
String str2 = " världen";
String str3 = str1 + str2;
Serial.println(str3);
```

Programmet kommer då att skriva ut "Hej världen" i seriemonitorn.

```Arduino
int num1 = 5;
String str4 = " har " + String(num1) + " äpplen.";
String str5 = str1 + str4;
Serial.println(str5);
```

Programmet kommer då att skriva ut "Hej har 5 äpplen." i seriemonitorn.

## Mer Information:

Historiskt sett fanns det ingen inbyggd metod att konkatenera strängar i tidigare versioner av Arduino, till exempel `StringConcatenate()`. Arduino-programmerare måste använda en omständlig metod med `sprintf`.

Alternativen till att konkatenera strängar i Arduino innebär att använda funktioner som `sprintf()` eller `strcat()`, men dessa kan vara mer komplicerade för nya programmerare att förstå och använda.

När det gäller implementeringsdetaljer är det viktigt att notera att Arduino använder dynamiskt skiftande minne när en sträng konkateneras. Det innebär att minnesanvändningen kan öka snabbt om man konkatenerar stora strängar, vilket kan leda till minnesproblem i Arduino-enheter.

## Se Även:

Relaterade resurser om ämnet inkluderar Arduinos officiella dokumentation, som tillhandahåller detaljerad information om strängmanipuleringsmetoder, och Arduino-forum där programmerare diskuterar olika tekniker och trix.

1. [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [Arduino Forum: Using Strings](https://forum.arduino.cc/t/a-guide-to-arduino-strings/65861)