---
title:    "Elm: Användning av reguljära uttryck"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Om du programmerar i Elm, är chansen stor att du har stött på strängmanipulering. En vanlig metod för att hantera detta är att använda reguljära uttryck, eller "regular expressions" på engelska. Här ska vi titta på anledningarna till varför det kan vara användbart att ha kunskap om reguljära uttryck och hur du kan använda dem i dina Elm-program.

## Så här gör du

Först och främst är det viktigt att veta vad reguljära uttryck är. De är en syntax för att söka och matcha mönster i strängar. I Elm kan du använda funktionen `Regex.find` för att söka igenom en sträng med hjälp av ett reguljärt uttryck. Här är ett exempel där vi söker efter ett exakt matchat ord:

```Elm
import Regex exposing (..)

find "elit" "Hej, detta är en text om Elite Hotel."
--> Just {match = "elit", index = 27, number = 0}

```

Om du vill ha mer flexibilitet kan du använda teckenklasser i ditt reguljära uttryck. De används för att matcha flera möjligheter, till exempel alla bokstäver eller siffror. Här är ett exempel där vi söker efter både stora och små bokstäver:

```Elm
import Regex exposing (..)

find "[A-Za-z]" "Hello, World!"
--> Just {match = "H", index = 0, number = 0}
```

## Djupdykning

Reguljära uttryck kan verka förvirrande och överväldigande till en början, men när du blir bekväm med dem kommer du upptäcka hur kraftfulla de är för att manipulera strängar. Här är några tips för att hjälpa dig på vägen:

- `Regex.attempt` kan användas för att hantera felaktiga reguljära uttryck. Det returnerar `Result` vilket är användbart för felhantering.
- `Regex.replace` kan användas för att ersätta delar av en sträng som matchar ett reguljärt uttryck. Detta är särskilt användbart när du vill filtrera eller formatera en sträng.
- Se till att lära dig vanliga teckenklasser, specialtecken och kvantifierare för att kunna bygga mer avancerade reguljära uttryck.

## Se även

- [Elm Regex Package](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Learn Regex The Hard Way (Engelska)](https://regex.learncodethehardway.org/)