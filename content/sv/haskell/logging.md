---
title:                "Loggning"
date:                  2024-01-26T01:07:08.828819-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning i programmering innebär i grund och botten att lämna ett spår av brödsmulor i form av registrerade händelser eller meddelanden, vilka kan användas för att spåra vad din applikation gör vid varje given tidpunkt. Programmerare gör det för att felsöka problem, övervaka systemets prestanda och granska beteende av säkerhets- och efterlevnadsskäl.

## Hur man gör:
I Haskell kan loggning implementeras med hjälp av bibliotek som `monad-logger` eller `hslogger`. Här är ett snabbt exempel med `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Startar applikationen..."
    liftIO $ putStrLn "Gör några kritiska arbeten..."
    logErrorN "Hoppsan! Något gick fel."

main :: IO ()
main = runStdoutLoggingT logExample

{- Sample Output
[Info] Startar applikationen...
Gör några kritiska arbeten...
[Error] Hoppsan! Något gick fel.
-}
```

Detta enkla exempel visar hur du kan strössla loggutskrifter genom din kod för att få insikt i vad som händer under körning. `logInfoN` och `logErrorN` används för att logga informations- och felmeddelanden respektive.

## Fördjupning:
Loggning har kommit långt från enkla utskriftsuttryck till sofistikerade loggningsramverk. Historiskt var loggar bara textutgångar till en konsol eller fil, men nu inkluderar de strukturerad data som kan parsas och analyseras av olika verktyg.

I Haskell kan loggning göras i en ren funktionell stil som innebär att loggaktioner passeras explicit eller genom att använda monadiska sammanhang för orenhet, där loggare implicit trådas genom beräkningen.

Biblioteket `hslogger`, till exempel, är mer traditionellt och mutabelt jämfört med `monad-logger`. `monad-logger` erbjuder integration med monadhögen och ger mer flexibilitet när det gäller utformatsuttryck och kontroll. Båda biblioteken tillåter dig att ställa in loggningsnivåer, vilket hjälper till att filtrera loggmeddelanden baserat på deras vikt. Loggningsnivåerna inkluderar debug, info, notice, warning, error, critical, alert och emergency.

Haskells synsätt på loggning sammanfaller ofta med dess betoning på typsäkerhet och renhet. Loggar kan hanteras på ett sådant sätt att även om loggningen misslyckas kommer det inte att orsaka att huvudapplikationen kraschar på grund av Haskells robusta felhanteringskapacitet.

## Se även:
- [`monad-logger` dokumentation på Hackage](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` paket på Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Kapitel 19, om felhantering](http://book.realworldhaskell.org/read/error-handling.html)
- [The Logging Facade for Haskell (log-base)](https://hackage.haskell.org/package/log-base)
