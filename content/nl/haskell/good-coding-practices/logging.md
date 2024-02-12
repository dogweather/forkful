---
title:                "Logboekregistratie"
aliases:
- /nl/haskell/logging/
date:                  2024-01-28T22:02:43.388177-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen in programmeren is in wezen een spoor van broodkruimels achterlaten in de vorm van opgenomen gebeurtenissen of berichten, die gebruikt kunnen worden om bij te houden wat je applicatie op elk gegeven moment doet. Programmeurs doen dit om problemen te debuggen, de systeemprestaties te monitoren en het gedrag te auditen voor beveiligings- en nalevingsredenen.

## Hoe:
In Haskell kan loggen geïmplementeerd worden met behulp van bibliotheken zoals `monad-logger` of `hslogger`. Hier is een snel voorbeeld met `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "De applicatie starten..."
    liftIO $ putStrLn "Bezig met wat kritiek werk..."
    logErrorN "Oeps! Er ging iets mis."

main :: IO ()
main = runStdoutLoggingT logExample

{- Voorbeelduitvoer
[Info] De applicatie starten...
Bezig met wat kritiek werk...
[Fout] Oeps! Er ging iets mis.
-}
```

Dit eenvoudige voorbeeld laat zien hoe je logberichten door je code heen kunt strooien om inzicht te krijgen in wat er tijdens de uitvoering gebeurt. `logInfoN` en `logErrorN` worden gebruikt om respectievelijk informatieve en foutberichten te loggen.

## Diepgaand:
Loggen is een lange weg gekomen van simpele printverklaringen tot aan geavanceerde logframeworks. Historisch gezien waren logs slechts tekstuitvoer naar een console of bestand, maar nu bevatten ze gestructureerde gegevens die door verschillende tools kunnen worden geparseerd en geanalyseerd.

In Haskell kan loggen op een puur functionele stijl worden gedaan, waarbij logacties expliciet worden doorgegeven of met behulp van monadische contexten voor onzuiverheid, waar loggers impliciet door de berekening worden geleid.

De `hslogger` bibliotheek is bijvoorbeeld traditioneler en veranderlijker vergeleken met `monad-logger`. `monad-logger` biedt integratie met de monad-stack en biedt meer flexibiliteit in termen van uitvoeropmaak en controle. Beide bibliotheken stellen je in staat om logniveaus in te stellen, wat helpt bij het filteren van logberichten op basis van hun belangrijkheid. Logniveaus omvatten debug, info, melding, waarschuwing, fout, kritiek, alarm en noodgeval.

De benadering van Haskell ten opzichte van loggen is vaak in lijn met de nadruk op typeveiligheid en zuiverheid. Logs kunnen op zo'n manier worden afgehandeld dat zelfs als het loggen faalt, het niet zal leiden tot een crash van de hoofdtoepassing vanwege de robuuste foutafhandelingsmogelijkheden van Haskell.

## Zie Ook:
- [`monad-logger` documentatie op Hackage](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` pakket op Hackage](https://hackage.haskell.org/package/hslogger)
- [Echt Wereld Haskell, Hoofdstuk 19, over Foutafhandeling](http://book.realworldhaskell.org/read/error-handling.html)
- [De Logging Gevel voor Haskell (log-base)](https://hackage.haskell.org/package/log-base)
