---
title:                "Arduino: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanlänka strängar är ett viktigt koncept inom Arduino programmering eftersom det tillåter oss att kombinera flera strängar till en enda sträng. Detta kan vara särskilt användbart när vi arbetar med text eller behöver skapa variabler för att lagra data.

## Hur man gör

Att sammanlänka strängar i Arduino är ganska enkelt. Nedan följer ett exempel på kod som visar hur man enkelt kan sammanlänka två strängar "Hej" och "världen" för att få ut strängen "Hej världen":

```Arduino
String första = "Hej";
String andra = "världen";
String sammanlänkad = första + " " + andra;
Serial.println(sammanlänkad);
```

Detta kommer att skriva ut "Hej världen" i seriell överföring.

## Djupdykning

Det finns olika sätt att sammanlänka strängar i Arduino, men det mest använda sättet är genom att använda operatorn "+" eller funktionen "concat()". Det är också viktigt att notera att sammanlänka en sträng med en heltal eller flyttal kräver att du först konverterar dem till en sträng med funktionen "String()". Detta kan vara användbart när vi behöver skriva ut våra sensorvärden tillsammans med en sträng för att ange vad som mäts.

Det är också möjligt att sammanlänka strängar i en loop eller inuti en if-sats för att skapa dynamiska strängar baserade på olika variabler.

## Se även

- [Arduino String Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial: Arduino for Beginners - How to Concatenate Strings](https://www.youtube.com/watch?v=1KvhSOvF8ps)
- [Guide: How to Combine Strings in Arduino](https://maker.pro/arduino/tutorial/how-to-combine-strings-in-arduino)