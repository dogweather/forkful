---
title:                "Att Göra En Sträng Med Stor Första Bokstav"
html_title:           "Arduino: Att Göra En Sträng Med Stor Första Bokstav"
simple_title:         "Att Göra En Sträng Med Stor Första Bokstav"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Innan vi dyker in i hur du kapitaliserar strängar i Arduino, låt oss först gå igenom varför du skulle vilja göra det. En vanlig anledning kan vara om du vill formatera texten på en LCD-skärm eller skriva ut den på en seriell monitor.

## Så här gör du

För att kapitalisera en sträng i Arduino, kan du använda funktionen `toUpperCase()` som finns tillgänglig för alla strängar. Här är ett exempel på hur du kan använda det i en kod:

```Arduino
String text = "hej!"; // Definiera en sträng
text = text.toUpperCase(); // Tillämpa toUpperCase() funktionen på strängen
Serial.println(text); // Skriv ut den kapitaliserade strängen på seriell monitor
```

Det här är vad som kommer att skrivas ut i seriell monitorn:

```
HEJ!
```

Som du kan se, har funktionen `toUpperCase()` konverterat alla bokstäver i strängen till stora bokstäver. Om du vill veta mer om hur funktionen fungerar, gå vidare till nästa avsnitt.

## Deep Dive

För att få en djupare förståelse av hur toUpperCase() funktionen fungerar, är det viktigt att förstå att alla strängar i Arduino är arrayer av `char` typen. En `char` kan representera en enda bokstav eller tecken, och varje bokstavs ASCII-värde kan ändras genom att lägga till eller subtrahera ett konstant värde. I ASCII-tabellen är värdet för en liten bokstav alltid 32 större än värdet för samma stora bokstav. Därför använder toUpperCase() funktionen `+ 32` operationen för att konvertera alla bokstäver till stora bokstäver.

## Se även

- [Arduino referens för toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [ASCII-tabell](https://www.asciitable.com/)