---
date: 2024-01-20 17:44:15.915338-07:00
description: "Hvordan gj\xF8r man det: Haskell gir deg verkt\xF8yene du trenger for\
  \ \xE5 laste ned nettsider rett fra koden din: `http-conduit`. La oss ta en kjapp\
  \ titt p\xE5\u2026"
lastmod: '2024-03-13T22:44:40.840016-06:00'
model: gpt-4-1106-preview
summary: "Haskell gir deg verkt\xF8yene du trenger for \xE5 laste ned nettsider rett\
  \ fra koden din."
title: Nedlasting av en nettside
weight: 42
---

## Hvordan gjør man det:
Haskell gir deg verktøyene du trenger for å laste ned nettsider rett fra koden din: `http-conduit`. La oss ta en kjapp titt på hvordan du implementerer dette:

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    if statusCode == 200
       then L8.putStrLn $ getResponseBody response
       else print statusCode
```
Kjør dette i `ghci` eller en Haskell-fil. Du får HTTP-responsen direkte i terminalen eller logger sidens innhold hvis statuskoden er OK (200).

## Dypdykk
Før `http-conduit` og andre moderne biblioteker ble populære, brukte Haskell-programmerere ofte lavnivå-biblioteker som `http`. Nøkkelen er effektivitet og bekvemmelighet; `http-conduit` håndterer HTTP-protokoller, omdirigeringer og mer med mindre manuell innsats.

Alternativer? Absolutt. `wreq` og `curl` er andre pakker som kan utføre lignende oppgaver. Implementasjonsdetaljene varierer, men prinsippet er det samme: Send en forespørsel, motta svar, behandle.

Det er verdt å nevne at å laste ned en nettside i Haskell kan innebære ekstra utfordringer med håndtering av bytes og tekstkoding - Haskell er strikt på typer og formater.

## Se Også
- HTTP-Conduit på Hackage: https://hackage.haskell.org/package/http-conduit
- "Real World Haskell" av Bryan O'Sullivan (nettbehandling): http://book.realworldhaskell.org/read/
- StackOverflow for problemstillinger og løsninger innen nettverksprogrammering i Haskell: https://stackoverflow.com/questions/tagged/haskell-network-programming
