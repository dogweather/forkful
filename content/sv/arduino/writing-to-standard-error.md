---
title:                "Skrivning till standardfel"
html_title:           "Arduino: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standard error på en Arduino är ett sätt för programutvecklare att snabbt få feedback och felsöka sin kod. Genom att skicka felmeddelanden till standard error istället för standard output, kan utvecklare enkelt identifiera och åtgärda problem i sin kod.

## Hur?
För att skriva till standard error på en Arduino, använd syntaxen `Serial.println()` och lägg till prefixet `err` efter punkten. Här är ett exempel:
```
Arduino Serial.println(err "Felmeddelande här");
```
Detta kommer att skriva ut felmeddelandet på en ny rad till standard error, som kan läsas i seriell monitor eller via USB.

## Djupdykning
Att skriva till standard error är ett vanligt sätt att hantera felmeddelanden inom programmering. I stora system där standard output används för andra syften, som att skriva till en loggfil eller kommunicera med en användare, är det viktigt att skilja mellan felmeddelanden och vanliga utskrifter. Att använda standard error gör detta möjligt genom att separera felen från den vanliga utdata.

Ibland kan programutvecklare använda sig av alternativ som `Serial.availableForWrite()` för att kontrollera om seriell kommunikation är tillgänglig innan de skriver till standard output. Men det är en längre och mer komplex metod jämfört med att skriva till standard error och kan bli svårt att underhålla i stora projekt.

En viktig detalj att notera är att Arduino-programmeringsspråket faktiskt är en implementering av språket Wiring. Det är värt att känna till eftersom vissa funktioner och syntax kan vara annorlunda jämfört med ren C++.

## Se även
Mer information om att skriva till standard error på en Arduino finns tillgänglig på [Arduino-hemsidan](https://www.arduino.cc/reference/en/language/functions/communication/serialprint/). För utvecklare som känner sig bekväma med att använda standard output för felmeddelanden, kan [WireDebug-biblioteket](https://github.com/Chris--A/WireDebug) vara en användbar lösning.