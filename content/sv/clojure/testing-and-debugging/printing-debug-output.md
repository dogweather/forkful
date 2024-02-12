---
title:                "Skriva ut felsökningsdata"
aliases:
- /sv/clojure/printing-debug-output/
date:                  2024-01-20T17:52:09.417331-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Utskrift av felsökningsdata är att få programmet att skriva ut vad det tänker på. Programmerare använder detta för att förstå vad som händer under huven och snabbt hitta kryphål.

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
