---
date: 2024-01-26 04:12:58.950669-07:00
description: "Hur man g\xF6r: REPL \xE4r central f\xF6r Lisps familjens interaktiva\
  \ utvecklingsfilosofi, och Clojure, en modern Lisp-dialekt, g\xF6r stor nytta av\
  \ detta verktyg.\u2026"
lastmod: '2024-04-05T22:50:51.811336-06:00'
model: gpt-4-0125-preview
summary: "REPL \xE4r central f\xF6r Lisps familjens interaktiva utvecklingsfilosofi,\
  \ och Clojure, en modern Lisp-dialekt, g\xF6r stor nytta av detta verktyg."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
Börja med att starta REPL:

```Clojure
user=> (println "Hej, REPL!")
Hej, REPL!
nil
```

Definiera en funktion och testa den:
```Clojure
user=> (defn greet [name] (str "Hej, " name "!"))
#'user/greet
user=> (greet "Clojure Programmerare")
"Hej, Clojure Programmerare!"
```

Experimentera med datastrukturer:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Djupdykning
REPL är central för Lisps familjens interaktiva utvecklingsfilosofi, och Clojure, en modern Lisp-dialekt, gör stor nytta av detta verktyg. Den går tillbaka till den första Lisp REPL på sent 1950-tal. Alternativ i andra språk inkluderar Pythons tolk och Nodes.js's konsol, men Clojures REPL har en förstklassig status och är integrerad i arbetsflödet.

En Clojure REPL-session kan integreras i olika miljöer som kommandoraden, IDE:s (som IntelliJ med Cursive, eller Emacs med CIDER) eller webbläsarbaserade verktyg som Nightcode. I en djupare bemärkelse, ger REPL utvecklaren möjlighet att manipulera språkets konstruktioner vid körning och bära tillstånd över olika transformationer, ofta leder det till utforskningsprogrammering och mer robust kod.

REPL:s funktionalitet lyser med verktyg som `lein repl` eller `clj`, vilket tillåter för hantering av beroenden, olika plugins och projektspecifika anpassningar, vilket leder till en mer produktiv och flexibel utvecklingsprocess.

## Se även
- Den officiella Clojure-webbplatsens guide om REPL: https://clojure.org/guides/repl/introduction
- Rich Hickeys prat om REPL-driven utveckling: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Praktisk Clojure: att använda REPL för iterativ utveckling: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
