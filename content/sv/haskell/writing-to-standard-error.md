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

## Varför
Att skriva till standard error kan vara användbart i vissa situationer, till exempel när man vill skicka felmeddelanden eller logga information som inte är avsedd att synas för användaren.

## Så här gör du
För att skriva till standard error i Haskell kan du använda funktionen `hPutStrLn` från modulen `System.IO`. Detta gör det möjligt att skriva en sträng till standard error, vilket vanligtvis är kopplat till terminalen. Nedan visas ett exempel på hur detta kan implementeras:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Det här är ett felmeddelande."
```

När detta program körs kommer felmeddelandet att skrivas till standard error, vilket i sin tur kan ses i terminalen. Om du istället vill skriva till standard output kan du använda funktionen `putStrLn` på samma sätt.

## Djupdykning
I Haskell finns det flera olika sätt att hantera felmeddelanden. Det är vanligt att använda antingen `Either` monad eller `IO` monad för att fånga och hantera fel. Att skriva till standard error är ett sätt att logga information om eventuella fel som uppstår i programmet. Det kan också vara användbart att låta felmeddelanden innehålla specifika detaljer som kan hjälpa till att hitta och åtgärda problemet.

## Se även
- [Officiell Haskell-dokumentation för hPutStrLn](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)
- [Haskellers-forumtråd om att skriva till standard error](https://mail.haskell.org/pipermail/haskell-cafe/2018-October/130515.html)