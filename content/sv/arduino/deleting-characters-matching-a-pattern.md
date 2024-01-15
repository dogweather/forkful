---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett visst mönster kan vara användbart i många olika situationer. Till exempel kan det hjälpa till att rensa en sträng från onödiga tecken, vilket gör den lättare att hantera och bearbeta.

## Hur man gör det
Det finns flera sätt att gå tillväga för att ta bort tecken som matchar ett visst mönster i Arduino. Ett sätt är att använda sig av den inbyggda funktionen "replace" som finns i "String" biblioteket. Se nedan för ett exempel på hur man kan implementera detta i sin kod:

```Arduino
String originalString = "Hej alla Arduino-användare!";
String modifiedString = originalString.replace("a", "");

Serial.println(modifiedString);
```
Detta kodblock kommer att ta bort alla "a" från den ursprungliga strängen och skriva ut resultatet på seriell monitor. Resultatet blir "Hej ll Arduno-nvändre!".

## Djupdykning
När man använder sig av "replace"-funktionen för att ta bort tecken som matchar ett mönster, är det viktigt att vara medveten om vad som händer bakom kulisserna. I grund och botten ersätts de matchande tecknen med en tom sträng, vilket innebär att den ursprungliga strängens längd förändras. Detta kan påverka prestandan i ens kod om man arbetar med stora och/eller många strängar.

Det finns också andra sätt att ta bort tecken som matchar ett mönster, som att använda sig av regelbundna uttryck eller loopa genom varje tecken i strängen och jämföra dem med det önskade mönstret. Det är viktigt att välja den lämpligaste metoden beroende på ens specifika behov och kodens krav.

## Se även
- [Arduino String replace dokumentation](https://www.arduino.cc/en/Curie/replace)
- [Guide till regelbundna uttryck i Arduino](https://learn.sparkfun.com/tutorials/regular-expressions/reg-ex-in-arduino)
- [Enkel Arduino kod för att ta bort mellanslag](https://www.maketecheasier.com/delete-spaces-string-arduino/)