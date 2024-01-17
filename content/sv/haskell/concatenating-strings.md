---
title:                "Sammanslagning av strängar"
html_title:           "Haskell: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontinuerligt sammanfoga strängar är en viktig del av programmering. Det betyder att kombinera två eller flera strängar till en enda sträng. Programmerare använder denna teknik för att skapa mer dynamiska och användbara texter och för att utföra olika manipulationer på strängar.

## Hur man gör:

```Haskell
"Hello " ++ "World!" -- Output: "Hello World!"
```

```Haskell
"2 + 2 is " ++ show (2 + 2) -- Output: "2 + 2 is 4"
```

## Djupdykning:

Historiskt sett har sammanfogning av strängar varit begränsad inom programmeringsspråken, men det har ändrats med introduktionen av Haskell-programmeringsspråket. Det finns också alternativa sätt att sammanfoga strängar, till exempel med formatsträngar eller funktioner som "join" i andra språk. I Haskell används operatorn "++" för sammanfogning av strängar, och det är också värt att nämna att dessa operatorer kan användas för att manipulera strängar på olika sätt, som att ta bort eller ersätta delar av en sträng.

## Se även:

Här är några länkar för mer information om hur man sammanfogar strängar och andra användbara manipuleringar av strängar:

- [Haskell dokumentation för strängar](https://www.haskell.org/tutorial/strings.html)
- [Jämförelse av olika metoder för att sammanfoga strängar i olika språk](https://lord.io/blog/2014/strconcat/)
- [Mer om manipulering av strängar i Haskell](https://wiki.haskell.org/Strings)