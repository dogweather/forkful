---
date: 2024-01-20 17:39:54.059651-07:00
description: "Hur g\xF6r man: Output skulle vara texten skriven i temp-filen, t.ex.\
  \ \"Hej fr\xE5n Clojure!\"."
lastmod: '2024-04-05T21:53:38.873156-06:00'
model: gpt-4-1106-preview
summary: Output skulle vara texten skriven i temp-filen, t.ex.
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## Hur gör man:
```Clojure
(require '[clojure.java.io :as io])

;; Skapa en temporär fil
(def temp-file (io/file (io/create-temp-file "prefix-" ".suffix")))

;; Använd temp-filen, här skriver vi bara lite text
(spit temp-file "Hej från Clojure!")

;; Läs från temp-filen
(println (slurp temp-file))

;; Rensa upp: ta bort temp-filen när du är klar
(.delete temp-file)
```
Output skulle vara texten skriven i temp-filen, t.ex. "Hej från Clojure!".

## Djupdykning
Temporära filer har varit ett verktyg i programmerares verktygslåda sedan de tidiga dagarna av datorutveckling. De är perfekta för att hantera stora datamängder som inte får plats i minnet, eller när du behöver en isolerad miljö för att testa filoperationer utan att riskera befintliga data. Alternativen till temporära filer kan inkludera användning av in-memory datastrukturer, som till exempel vectors eller hashtabeller i Clojure, men dessa är inte alltid praktiska eller möjliga. Implementationen av temporära filer i Java, som Clojure är byggt ovanpå, säkerställer att filerna är säkra och unika, och att de ofta kommer att raderas automatiskt när programmet som skapade dem avslutas.

## Se även
- Clojure Documentation: https://clojure.org/api/api
- Java's File API (som Clojure använder): https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Guide to `java.io`: https://www.baeldung.com/java-io
