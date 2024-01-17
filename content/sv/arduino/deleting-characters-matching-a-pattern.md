---
title:                "Radering av tecken som matchar ett mönster"
html_title:           "Arduino: Radering av tecken som matchar ett mönster"
simple_title:         "Radering av tecken som matchar ett mönster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster är en vanlig programmeringsteknik för att kunna manipulera textsträngar på ett effektivt sätt. Det används ofta för att ta bort oönskade tecken eller för att söka efter specifika mönster i en text.

## Hur man gör:
Här är ett enkelt exempel på hur du kan radera alla siffror från en textsträng:
```Arduino
String text = "Hej123 vad456";
for (int i = 0; i < text.length(); i++) {
  if (isdigit(text.charAt(i))) {    // kollar om tecknet är en siffra
    text.remove(i,1);               // tar bort tecknet på positionen
    i--;                            // korrigerar loopen efter borttagningen
  }
}
Serial.println(text);               // skriver ut "Hej vad"
```
I detta exempel använder vi en for-loop för att gå igenom varje tecken i textsträngen och kontrollera om det är en siffra med hjälp av funktionen `isdigit()`. Om det är en siffra, använder vi funktionen `remove()` för att ta bort tecknet från textsträngen och korrigerar sedan loopen så att den fortsätter på rätt position.

## Djupdykning:
Att radera tecken som matchar ett mönster kan spåras tillbaka till programmeringsspråket C, där man använde funktionen `strpbrk()` för att hitta positionen för det första tecknet som matchade ett visst mönster. I andra moderna programmeringsspråk finns det inbyggda funktioner för att radera tecken från textsträngar, såsom `trim()` i Java.

Alternativ till att radera tecken är att ersätta dem med andra tecken, eller att använda funktioner för att söka efter specifika mönster och sedan manipulera textsträngen baserat på det. Det är också möjligt att använda reguljära uttryck (regular expressions) för mer avancerade mönstermatchning.

Implementeringen av att radera tecken som matchar ett mönster kan variera beroende på programmeringsspråk och plattform, men konceptet är detsamma. Det kräver en loop för att gå igenom varje tecken i textsträngen och en mekanism för att ta bort eller ersätta tecknen baserat på ett visst mönster.

## Se även:
- [Dokumentation för Arduino String Class](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Information om reguljära uttryck i programmering](https://www.regular-expressions.info/)