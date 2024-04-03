---
date: 2024-01-20 17:38:01.461622-07:00
description: "Omforme en streng til sm\xE5 bokstaver handler om \xE5 konvertere alle\
  \ bokstavene i en gitt streng til deres sm\xE5bokstavversjoner. Programmerere gj\xF8\
  r dette for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.390052-06:00'
model: gpt-4-1106-preview
summary: "Omforme en streng til sm\xE5 bokstaver handler om \xE5 konvertere alle bokstavene\
  \ i en gitt streng til deres sm\xE5bokstavversjoner."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## What & Why?
Omforme en streng til små bokstaver handler om å konvertere alle bokstavene i en gitt streng til deres småbokstavversjoner. Programmerere gjør dette for å standardisere tekstdata, som i søk eller sammenligninger, hvor store og små bokstaver bør behandles likt.

## How to:
Clojure gir deg verktøyene du trenger for å enkelt gjøre om strenger til små bokstaver. `clojure.string/lower-case` er din venn her. Her er et eksempel:

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Hei Verden!")
; => "hei verden!"
```

Sample output blir "hei verden!" etter å ha brukt `lower-case` funksjonen på strengen "Hei Verden!".

## Deep Dive
Å konvertere tekst til små bokstaver er ikke noe nytt. Tradisjonelt har det vært viktig i programmering for å unngå case-sensitivitet i tekstbehandling. I historisk kontekst har forskjellige programmeringsspråk laget sine egne funksjoner for å håndtere dette.

Clojure, som er et moderne Lisp-dialekt, tilbyr standardbibliotek funksjonen `clojure.string/lower-case` for slik konvertering. Det er også alternative måter å gjøre dette på, som å benytte Java's innebygde metoder gjennom Java Interop (gjennomgripende samhandling mellom Clojure og Java), men for ren Clojure kode, er `lower-case` den rette veien å gå.

Det som er interessant med Clojure sin implementasjon, er hvordan det er designet for å jobbe med JVM (Java Virtual Machine) og håndterer dermed også alle Unicode-tegn korrekt.

## See Also
- Clojure's string API documentation: [clojure.github.io/clojure/clojure.string-api.html](https://clojure.github.io/clojure/clojure.string-api.html)
- A guide to Clojure strings and Interop with Java: [clojure.org/guides/weird_characters](https://clojure.org/guides/weird_characters)
- Learn more about Unicode and text standardization: [unicode.org/standard/standard.html](https://www.unicode.org/standard/standard.html)
