---
date: 2024-01-20 17:52:08.301120-07:00
description: "How to: I gamle dager, da skjermer var luksus, ble feils\xF8king gjort\
  \ ved \xE5 analysere kodelister og tolke blinkende lys. Utskrift for feils\xF8king\
  \ er et steg\u2026"
lastmod: '2024-04-05T21:53:41.383442-06:00'
model: gpt-4-1106-preview
summary: "I gamle dager, da skjermer var luksus, ble feils\xF8king gjort ved \xE5\
  \ analysere kodelister og tolke blinkende lys."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## How to:
```Clojure
;; Skriv ut en enkel melding til konsollen
(println "Heisann! Her er noe debug-info:")

;; Utskrift av en variabels verdi
(def x 42)
(println "Verdien av x er:" x)

;; Utskrift med formatert tekst
(printf "Verdien av %s er: %d" 'x x)

;; Utskrift for å visualisere en rekke verdier
(doseq [i (range 5)]
  (println i))

;; Eksempel på output
; Heisann! Her er noe debug-info:
; Verdien av x er: 42
; Verdien av x er: 42
; 0
; 1
; 2
; 3
; 4
```

## Deep Dive:
I gamle dager, da skjermer var luksus, ble feilsøking gjort ved å analysere kodelister og tolke blinkende lys. Utskrift for feilsøking er et steg opp – det gjør livet lettere og koden mer tilgjengelig. Clojure, en moderne Lisp-dialekt, arver denne praksisen og gjør det enkelt med `println` og `printf`. Alternativer inkluderer feilsøkingsverktøy som nREPL og avanserte logging-biblioteker, som gir mer kontroll og struktur – de er verdt å sjekke ut når prosjektene blir større og mer komplekse.

Implementasjonsdetaljer: `println` lener seg på Java's System.out.println og `printf` på Java's Formatter, da Clojure kjører på JVM (Java Virtual Machine). Koden kompileres til Java bytecode, så forståelsen av Java's verktøy kan forbedre evnen til å feilsøke effektivt i Clojure.

## See Also:
- [Clojure's println](https://clojuredocs.org/clojure.core/println)
- [Clojure's printf](https://clojuredocs.org/clojure.core/printf)
- [nREPL, a Clojure debugging tool](https://nrepl.org/nrepl/index.html)
- [Timbre, a Clojure logging library](https://github.com/ptaoussanis/timbre)
