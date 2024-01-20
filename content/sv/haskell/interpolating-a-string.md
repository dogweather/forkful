---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Stränginterpolation är en metod för att infoga variabler i strängar direkt. Detta gör kod lättare att läsa, skriva och underhålla.

## Så här gör du:

Haskell kräver `Text.Printf` modulen för stränginterpolation. Nedan visas några enkla exempel:

```Haskell
import Text.Printf

name = "Ada"
-- Skriver ut "Hej, Ada!"
printf "Hej, %s!" name
```

Här, `%s `är ett format especificator som berättar var och hur att sätta in vår variabel.


## Djupdykning

Historiskt sett kommer stränginterpolation från Perl och Shell programmeringsspråk. I Haskell finns det några alternativ till `Text.Printf` för mer avancerade fall, till exempel `Data.Text` och `Text.InterpolatedString.Perl6`.

Under huven, funktioner som `printf` i Haskell genomförer stränginterpolation genom att parse format-strängen vid körtid, vilket skapar högre prestanda överbelastning jämfört till många andra språk.

## Se Även

För mer detaljerad diskussion om stränginterpolation i Haskell, besök följande länkar:

- Haskell Café: [String Interpolation i Heterogen List](https://mail.haskell.org/pipermail/haskell-cafe/2010-February/073593.html)