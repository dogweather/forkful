---
date: 2024-01-26 04:18:17.467940-07:00
description: "Bruk av et interaktivt skall, eller en Les-Evaluer-Skriv Ut-l\xF8kke\
  \ (REPL), lar deg kode interaktivt. Programmerere bruker det til raskt \xE5 teste\
  \ Swift-\u2026"
lastmod: '2024-03-13T22:44:41.143428-06:00'
model: gpt-4-0125-preview
summary: "Bruk av et interaktivt skall, eller en Les-Evaluer-Skriv Ut-l\xF8kke (REPL),\
  \ lar deg kode interaktivt."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hva og hvorfor?
Bruk av et interaktivt skall, eller en Les-Evaluer-Skriv Ut-løkke (REPL), lar deg kode interaktivt. Programmerere bruker det til raskt å teste Swift-snutter, feilsøke eller lære språket.

## Hvordan:
Start REPL ved å åpne en terminal og kjøre `swift`. Skriv kode direkte og trykk Enter for å kjøre den. Her er en smakebit:

```Swift
1> let hilsen = "Hallo, REPL!"
hilsen: String = "Hallo, REPL!"
2> print(hilsen)
Hallo, REPL!
```

Avslutt med `:quit` eller `Control-D`.

## Dypdykk
REPLs røtter går helt tilbake til Lisp-tolkere på 60-tallet. Swifts REPL står på skuldrene til LLVM, et kraftig kompileringsrammeverk, og tilbyr mer enn bare grunnleggende tolkning - det er et fullverdig verktøy med autofullføring, feilsøking og mer. REPL er flott for læring eller prototyping, men det er ikke et selvstendig utviklingsmiljø. Noen foretrekker å bruke Playgrounds i Xcode for en mer grafisk, filbasert tilnærming, mens andre holder seg til tradisjonell skriptredigering og kjøring.

Under hetten kompilerer Swifts REPL dynamisk kode til maskinspråk og eksekverer den, noe som er hvorfor den er relativt rask. Den kan også få tilgang til hvilke som helst kompilerte Swift-moduler, eller til og med C-biblioteker, noe som gjør den ganske kraftig. Merk, dog, at ikke alt fungerer perfekt i REPL; noen Swift-funksjoner, spesielt de som krever komplekse prosjektoppsett eller storyboard-filer, vil ikke fungere her.

## Se også
- [Swift.org - Komme i gang](https://www.swift.org/getting-started/#using-the-repl)
- Apples [Introduksjon til Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM-prosjektet](https://llvm.org/)
