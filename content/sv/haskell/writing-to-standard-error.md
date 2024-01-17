---
title:                "Skriva till standardfel"
html_title:           "Haskell: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standard error är ett sätt för programmerare att skicka meddelanden direkt till felutmatningen istället för den vanliga utmatningen. Detta är användbart när man vill separera olika typer av utmatningar, till exempel felmeddelanden från vanlig programoutput. 

## Hur?

```Haskell
import System.IO
hPutStr stderr "Felmeddelande: division med noll"
```

Output:
```
Felmeddelande: division med noll
```

## Djupdykning

I historisk kontext var standard error en del av Unix-systemet och tillät utskrifter på skärmen samtidigt som programmet kördes. Numera är standard error en standardiserad kommunikationskanal för felmeddelanden mellan program och användare. Det finns också alternativ till att använda standard error, som till exempel att logga felmeddelanden istället. Implementeringen av standard error varierar beroende på programmeringsspråk och operativsystem, men implementeringen i Haskell är standardiserad och stöds av System.IO modulen.

## Se även

- [HDocs för System.IO modulen](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [En guide till hantering av fel i Haskell](https://mmhaskell.com/blog/2017/4/3/throwing-errors-and-handling-expectations)
- [Ett diskussionsforum om Haskell](https://www.reddit.com/r/haskell/)