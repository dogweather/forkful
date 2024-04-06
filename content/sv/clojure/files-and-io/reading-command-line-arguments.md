---
date: 2024-01-20 17:55:54.398186-07:00
description: "Hur g\xF6r man: Kommandoradsargument har varit en grundsten i program\
  \ sedan de tidigaste dagarna av datorer. I Clojure, som i m\xE5nga Lisp-dialekter,\
  \ ger den\u2026"
lastmod: '2024-04-05T21:53:38.869143-06:00'
model: gpt-4-1106-preview
summary: Kommandoradsargument har varit en grundsten i program sedan de tidigaste
  dagarna av datorer.
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Hur gör man:
```Clojure
; För att starta, anta att du sparar detta i en fil som heter `cli-args.clj`

; Använd `*command-line-args*` för att fånga argumenten som en lista
(def args *command-line-args*)

; Exempel för att skriva ut alla argument
(doseq [arg args]
  (println arg))

; Kör programmet: `clojure cli-args.clj Hej Värld!`
; Förväntad utskrift:
; Hej
; Värld!
```

## Fördjupning
Kommandoradsargument har varit en grundsten i program sedan de tidigaste dagarna av datorer. I Clojure, som i många Lisp-dialekter, ger den globala variabeln `*command-line-args*` en enkel väg till argumenten. 

Alternativ till `*command-line-args*` inkluderar att använda bibliotek som `tools.cli`, som tillhandahåller mer avancerad funktionalitet för argumenttolkning, såsom flaggor och optionsparsing. 

Implementationen är plattformsoberoende men beror på Java Virtual Machine (JVM), eftersom Clojure är ett JVM-språk. Kommandoradsargumenten passeras därmed på samma sätt som de skulle till en Java-applikation.

## Se även
- Clojure's [`*command-line-args*` dokumentation](https://clojuredocs.org/clojure.core/*command-line-args*)
- [tools.cli GitHub-sida](https://github.com/clojure/tools.cli), för mer avancerad argumenttolkning
- [Clojure - Få hjälp och lära dig mer](https://clojure.org/community/resources)
