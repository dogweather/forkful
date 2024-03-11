---
date: 2024-01-26 01:06:27.936384-07:00
description: "Logging i programmering inneb\xE6rer i bunn og grunn \xE5 etterlate\
  \ seg et spor av \"smuler\" i form av registrerte hendelser eller meldinger, som\
  \ kan brukes til\u2026"
lastmod: '2024-03-11T00:14:14.410939-06:00'
model: gpt-4-1106-preview
summary: "Logging i programmering inneb\xE6rer i bunn og grunn \xE5 etterlate seg\
  \ et spor av \"smuler\" i form av registrerte hendelser eller meldinger, som kan\
  \ brukes til\u2026"
title: "Loggf\xF8ring"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging i programmering innebærer i bunn og grunn å etterlate seg et spor av "smuler" i form av registrerte hendelser eller meldinger, som kan brukes til å spore hva applikasjonen din gjør i et gitt øyeblikk. Programmerere gjør dette for å feilsøke problemer, overvåke systemets ytelse, og revidere oppførsel for sikkerhets- og overholdelsesgrunner.

## Hvordan:
I Haskell kan logging implementeres ved hjelp av biblioteker som `monad-logger` eller `hslogger`. Her er et kjapt eksempel som bruker `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Starter applikasjonen..."
    liftIO $ putStrLn "Utfører noe kritisk arbeid..."
    logErrorN "Oisann! Noe gikk galt."

main :: IO ()
main = runStdoutLoggingT logExample

{- Eksempel på utdata
[Info] Starter applikasjonen...
Utfører noe kritisk arbeid...
[Feil] Oisann! Noe gikk galt.
-}
```

Dette enkle eksempelet demonstrerer hvordan du kan strø kodeloggerklæringer gjennom koden din for å få innsikt i hva som skjer under kjøretiden. `logInfoN` og `logErrorN` brukes for å logge informasjons- og feilmeldinger henholdsvis.

## Dykk dypere:
Logging har kommet langt fra enkle utskrifter til konsoll eller filer til sofistikerte loggingsrammeverk. Historisk var logger bare tekstutdata til en konsoll eller fil, men nå inkluderer de strukturerte data som kan analyseres ved hjelp av forskjellige verktøy.

I Haskell kan logging gjøres i en ren funksjonell stil der loggehandlinger eksplisitt sendes videre, eller ved å bruke monadiske kontekster for urenhet, hvor loggerne er implisitt trukket gjennom beregningen.

Biblioteket `hslogger`, for eksempel, er mer tradisjonelt og mutabelt sammenlignet med `monad-logger`. `monad-logger` tilbyr integrasjon med monad-stakken og gir mer fleksibilitet i form av utdataformatering og kontroll. Begge bibliotekene lar deg sette loggnivåer, som hjelper til med å filtrere loggmeldinger basert på deres viktighet. Loggnivåer inkluderer debug, info, notice, warning, error, critical, alert, og emergency.

Haskells tilnærming til logging er ofte i tråd med dets vektlegging på type sikkerhet og renhet. Logger kan håndteres slik at selv om loggingen feiler, vil det ikke føre til at hovedapplikasjonen krasjer grunnet Haskells robuste feilhåndteringsevner.

## Se også:
- [`monad-logger` dokumentasjon på Hackage](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` pakken på Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Kapittel 19, om feilhåndtering](http://book.realworldhaskell.org/read/error-handling.html)
- [Logging Fasaden for Haskell (log-base)](https://hackage.haskell.org/package/log-base)
