---
date: 2024-01-20 17:52:09.417331-07:00
description: "Hur g\xF6r man: I f\xF6rhistorisk tid av programmering str\xE4ckte sig\
  \ fels\xF6kning till att gr\xE4va igenom pappersutskrifter. Idag \xE4r `println`\
  \ i Clojure enkelt och\u2026"
lastmod: '2024-04-05T21:53:38.855497-06:00'
model: gpt-4-1106-preview
summary: "I f\xF6rhistorisk tid av programmering str\xE4ckte sig fels\xF6kning till\
  \ att gr\xE4va igenom pappersutskrifter."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Hur gör man:
```Clojure
; Skriv ut en enkel sträng
(println "Hej, jag felsöker!")

; Utskriva variabler och strängar
(defn debug-var [var]
  (println "Debug: " var))

(debug-var "något viktigt")

; Format output with str
(println (str "Resultat: " (+ 2 2)))

; Resultat i REPL
; Hej, jag felsöker!
; Debug: något viktigt
; Resultat: 4
```

## Djupdykning:
I förhistorisk tid av programmering sträckte sig felsökning till att gräva igenom pappersutskrifter. Idag är `println` i Clojure enkelt och rakt på sak, men det är en dålig vana att lämna utskrifter i släppt kod. Alternativ inkluderar loggningsbibliotek som `timbre`, vilka erbjuder kontroll över loggningsnivåer. Clojure’s `println` släpper ut till standard output och kan omdirigeras eller tystas.

## Se även:
- Clojure's officiella dokumentation för `println`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/println
- Timbre, ett populärt loggningsbibliotek för Clojure: https://github.com/ptaoussanis/timbre
- Guide till effektiv felsökning i Clojure: https://clojure.org/guides/repl/debugging
