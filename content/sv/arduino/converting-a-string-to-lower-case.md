---
title:                "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

I många Arduino-programmeringsprojekt kan det vara nödvändigt att omvandla en sträng till små bokstäver. Detta kan vara användbart för att kunna jämföra och hantera olika ord eller för att skapa en enhetlig typografi i ett textmeddelande. Genom att konvertera strängar till små bokstäver kan du också undvika frustrerande problem med att jämna ihop orden ordentligt. I denna bloggpost kommer vi att titta på hur du enkelt kan konvertera en sträng till små bokstäver genom att använda Arduino.

## Så här gör du

Arduino har ett inbyggt bibliotek för strängmanipulering, vilket gör det enkelt att konvertera en sträng till små bokstäver. Det första steget är att deklarera en variabel för den ursprungliga strängen som du vill konvertera. Sedan kan du använda funktionen `toLowerCase()` för att omvandla strängen till små bokstäver. Här är ett enkelt exempel på hur det kan se ut i kod:

```Arduino
String originalString = "HEJ SWEDEN!";
String lowerCaseString = originalString.toLowerCase();
Serial.println(lowerCaseString);
```

När du laddar upp koden till din Arduino och öppnar seriell monitor kommer du att se att den ursprungliga strängen "HEJ SWEDEN!" har konverterats till små bokstäver och skrivits ut som "hej sweden!".

Det är viktigt att notera att funktionen `toLowerCase()` endast fungerar på ASCII-tecken. Om du använder speciella tecken som å, ä, ö eller andra icke-ASCII-tecken i din sträng kommer de inte att konverteras. För att kunna hantera icke-ASCII-tecken kan du använda funktioner som `toLower()` eller `toLowercase()` från andra bibliotek eller skriva din egen kod för att hantera konverteringen.

## Deep Dive

För de som är intresserade av mer detaljerad information om konvertering av strängar till små bokstäver, är det viktigt att förstå hur ASCII-koderna fungerar. ASCII-koderna är numeriska representationer av tecken och bokstäver som används för att kommunicera mellan datorer. Varje bokstav har en specifik numerisk kod, och för bokstäverna a-z är den stora bokstaven alltid 32 enheter högre än den lilla bokstaven. Så till exempel är ASCII-koden för "a" 97 och ASCII-koden för "A" är 65. Genom att lägga till eller subtrahera 32 från denna kod kan du enkelt konvertera en sträng till små bokstäver.

Det är också viktigt att tänka på att vissa bokstäver har olika ASCII-koder beroende på vilket språk som används. Så om du planerar att använda konverteringsfunktioner i dina projekt, se till att du har rätt kod för det specifika språket som du arbetar med.

## Se även

- [Arduino String Library](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [ASCII Character Codes](http://www.asciitable.com/)
- [UTF-8 Encoding](https://www.utf8-chartable.de/)