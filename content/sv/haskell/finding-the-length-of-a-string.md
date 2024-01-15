---
title:                "Att hitta längden av en sträng"
html_title:           "Haskell: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en viktig funktion inom programmering, speciellt när man arbetar med textbaserade data. Det kan vara användbart för uträkningar, jämförelser och bearbetning av data.

## Så här gör du
För att hitta längden på en sträng i Haskell kan du använda funktionen `length`. Här är ett exempel:

```Haskell
length "Hej, vad gör du?"
```

Detta kommer att returnera längden på strängen "Hej, vad gör du?" vilket är 16. 

För att använda `length` på en variabel som innehåller en sträng kan du skriva:

```Haskell
let sträng = "Välkommen till Haskell!"
length sträng
```

Detta kommer att returnera längden på strängen som är 24.

## Djupdykning
För att förstå hur `length` funktionen faktiskt fungerar i Haskell är det viktigt att förstå grundläggande koncept inom programmering. Haskell är ett "funktionellt" programmeringsspråk vilket betyder att funktioner betraktas som grundläggande byggstenar och kan användas och manipuleras som vilken annan datatyp som helst.

I fallet med `length` funktionen, tar den en sträng som inmatning och returnerar en heltal. När du tillämpar funktionen på en variabel eller hårdkodad sträng som vi gjorde ovan, utför `length` funktionen en iteration över varje tecken i strängen och räknar antalet tecken. Detta betyder att längden på en sträng i Haskell är representerad av en heltal.

## Se även
- [Haskell: En introduktion](https://www.pragmafusion.com/programmering/haskell-en-introduktion)
- [Funktionell programmering i praktiken med Haskell](https://blog.taqtiqa.com/functional-programming-in-practice-with-haskell-73e0e56d3ed9)
- [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/)