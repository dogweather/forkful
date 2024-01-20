---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive ut debug-utdata er en teknikk hvor programmerere henter informasjon fra programmet under kjøring. Dette gjøres for å identifisere og løse feil, samt forstå tilstanden til programmet i et gitt tidspunkt.

## Hvordan gjør man det:

Vi kan bruke "Debug.Trace" biblioteket for dette formålet. La oss vise det med et kodestykke:

```Haskell
import Debug.Trace

main = do
    let lista = [1..5]
    print $ traceShowId lista
```

Når du kjører dette, vil du se:

```
[1,2,3,4,5]
[1,2,3,4,5]
```

Her, `traceShowId lista` skriver ut verdien av `lista`, og returnerer den samme verdien.

## Dypdykk:

1. Historisk Kontekst: Bruken av "Debug.Trace" i Haskell ble ikke bredt akseptert før i senere versjoner av språket, til tross for dets brukervennlighet og effektivitet.
2. Alternativer: Noen programmerere foretrekker å benytte mer omfattende debuggingverktøy, slik som Haskell Debugger (Hoed) eller lokkende biblioteker som io-spy.
3. Implementeringsdetaljer: `traceShowId` fungerer ved å legge til en "utskriftseffekt" i programmet ditt, men endrer ikke selve oppførselen til koden din.

## Se Også:

For mer informasjon om debugging i Haskell, ta en titt på disse ressursene:

- "Real World Haskell" Kapittel 25: Debugging – https://book.realworldhaskell.org/read/debugging-and-profiling.html
- HaskellWiki: Debugging - https://wiki.haskell.org/Debugging
- Introduksjon til Hoed Debugger – https://wiki.haskell.org/Hoed
- io-spy biblioteket - https://hackage.haskell.org/package/io-spy