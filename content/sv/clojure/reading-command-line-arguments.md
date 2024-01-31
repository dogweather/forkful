---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:55:54.398186-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument innebär att man hämtar och använder de värden som skickas till ett program vid start. Programmerare gör detta för att tillåta användare att påverka programmets beteende utan att ändra koden.

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
