---
title:    "Haskell: Att starta ett nytt projekt."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt programmeringsprojekt i Haskell kan verka överväldigande för många. Men med detta funktionsrika och eleganta programmeringsspråk kan du bygga robusta och effektiva programvaror. Så varför inte ge det en chans?

## Hur man gör

Först och främst behöver du ladda ner och installera Haskell-plattformen på din dator. Detta gör du genom att gå till haskell.org och välja den lämpliga versionen för ditt operativsystem. När installationen är klar kan du öppna din favoriteditor och börjar koda!

För att visa ett enkelt exempel på hur Haskell fungerar, ska vi skapa en funktion som räknar ut kvadraten på ett tal:

```Haskell
square :: Int -> Int
square x = x * x
```

I koden ovan definierar vi en funktion som heter "square" som tar in en integer och returnerar en integer. Funktionen multiplicerar inparametern med sig själv och returnerar resultatet. 

För att testa funktionen kan vi skriva följande i en terminal:

```Haskell
square 5
```

Outputen blir då 25, eftersom vi matade in 5 som inparameter.

## Djupdykning

För att verkligen komma igång med ett nytt Haskell-projekt är det viktigt att förstå hur man strukturerar koden på ett smidigt sätt. Haskell använder sig av moduler för att organisera kod och förhindra namnkollisioner. Du kan tänka på en modul som en samling av relaterade funktioner, datatyper och värden.

Ett annat viktigt koncept i Haskell är "typklasser". Dessa liknar interfaces i andra programmeringsspråk och definierar vissa beteenden som en typ måste ha för att kunna användas på ett visst sätt.

Slutligen är det viktigt att förstå konceptet av "lazy evaluation" i Haskell. Detta innebär att koden inte utvärderas omedelbart utan väntar tills resultatet behövs. Detta gör att programmen i Haskell kan vara mer effektiva och undviker onödig beräkning.

## Se även

- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)