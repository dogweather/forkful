---
title:                "Gör en sträng versal"
html_title:           "Arduino: Gör en sträng versal"
simple_title:         "Gör en sträng versal"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Arduino-programmering: Capitalize a String

## Vad och Varför?

Att göra om en sträng till stora bokstäver, eller "capitalizing", innebär att varje bokstav i strängen skrivs om till dess stora motsvarighet. Programmerare gör detta för att jämna ut textdata och för att möjliggöra jämförelser utan att behöva ta hänsyn till små och stora bokstäver.

## Så här gör du:

Arduino erbjuder en funktion för att skifta till stora bokstäver i en sträng. Låt oss ta en titt på ett exempel.

```Arduino
String myStr = "hej världen!";
myStr.toUpperCase();
Serial.println(myStr); //Skriver ut: "HEJ VÄRLDEN!"
```
I detta exempel gör `toUpperCase()`-metoden alla bokstäver i stringen stora.

## Djupdykning

Historiskt sett gällde behovet av att skifta mellan små och stora bokstäver tidiga datorsystem där skillnaden mellan stor och liten bokstav kunde vara skillnaden mellan två helt olika kommandon.

Ett alternativ till `toUpperCase()` skulle vara att använda ett "loop" för att gå igenom varje karaktär i strängen individuellt, men den inbyggda metoden är både snabbare och enklare att använda.

I Arduino är `toUpperCase()` implementerad så att den itererar över varje tecken i strängen, kontrollerar om det är en liten bokstav, och om det är, byter den till stora bokstäver.

## Se även

För mer detaljer och alternativa metoder, se följande resurser:
- [Arduino String Reference:](https://www.arduino.cc/en/Reference/String)
- [Comparison of string methods:](https://www.arduino.cc/en/Tutorial/StringComparisonOperators)
- [Arduino Forum Discussion on String Capitalization:](https://forum.arduino.cc/index.php?topic=181847.0)