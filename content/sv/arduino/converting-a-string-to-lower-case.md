---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig operation inom programmering och kan vara användbart för att jämföra strängar eller för att se till att en sträng har en enhetlig formatering.

## Så här gör du

För att konvertera en sträng till gemener i Arduino, kan du använda den inbyggda funktionen `toLowerCase()`. Här är ett exempel på hur du kan använda den:

```Arduino
String myString = "HEJ!";
String lowerString = myString.toLowerCase();
Serial.println(lowerString);
```

Detta kommer att skriva ut `hej!` på serieporten.

## Djupdykning

När du använder `toLowerCase()` funktionen i Arduino, är det viktigt att notera att den endast fungerar för ASCII-tecken (tecken med värden mellan 0-127). Om du behöver konvertera strängar med icke-ASCII-tecken, kan du använda en funktion som `toLowerCaseLatin1()` från biblioteket "Arduino String".

En annan viktig sak att komma ihåg är att `toLowerCase()` skapar en ny sträng och returnerar den, istället för att ändra den befintliga strängen. Så om du vill spara den konverterade strängen, som i exemplet ovan, måste du tilldela resultatet till en ny strängvariabel.

## Se även

Här är några användbara länkar för att lära dig mer om konvertering av strängar i Arduino:

- [Arduino Reference - toLowerCase()](https://www.arduino.cc/reference/en/language/functions/string-functions/tolowercase/)
- [Arduino String Library](https://www.arduino.cc/reference/en/libraries/string/)
- [ASCII-tecken](https://www.ascii-code.com/)