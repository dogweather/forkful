---
title:                "Att påbörja ett nytt projekt"
html_title:           "Clojure: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt inom programmering handlar om att börja arbeta med en ny idé eller uppgift. Det gör programmören för att utveckla och förbättra sina färdigheter, skapa något nytt och utmana sig själv.

## Hur?

Ett nytt projekt kan skapas i Clojure genom att använda kommandot `lein new <project-name>`. Detta kommer att skapa en ny mapp med projektet och dess struktur. Sedan kan det öppnas i en editor som VSCode där man kan börja koda. Nedan följer ett exempel på hur man kan skapa en ny funktion i Clojure:

```Clojure
(defn add [a b]
  (+ a b))

(add 2 3) ;; Output: 5
```

I exemplet skapar vi en funktion som heter `add` som tar emot två parametrar och returnerar summan av dem. Sedan anropar vi funktionen och skriver ut resultatet.

## Deep Dive

Clojure är ett funktionellt programmeringsspråk som utvecklades av Rich Hickey år 2007. Det är baserat på Lisp och är känd för sin enkelhet och skalbarhet. Alternativ till Clojure inkluderar andra funktionella språk som Haskell och Scala. För att implementera ett nytt projekt i Clojure behöver man först installera Leiningen, ett byggverktyg som används för att hantera projektstrukturer och beroenden. Man kan också använda sig av en editor som är speciellt anpassad för Clojure, som till exempel Emacs eller Cursive för IntelliJ.

## Se även

För mer information om Clojure och hur man använder det kan du besöka följande länkar:

- Officiell hemsida: https://clojure.org/
- Leiningen: https://leiningen.org/
- Cursive: https://cursive-ide.com/