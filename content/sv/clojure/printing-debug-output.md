---
title:                "Skriva ut felsökningsutdata"
html_title:           "Clojure: Skriva ut felsökningsutdata"
simple_title:         "Skriva ut felsökningsutdata"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

Vad & Varför?
Debuggningsutskrift är ett sätt att visa information under körning av programmet för att hjälpa till att identifiera och lösa felaktigheter. Det är ett vanligt verktyg för programmerare att använda för att spåra och felsöka problem i sin kod.

Hur man:
```Clojure
(defn add [a b]
  (println "Adding" a "and" b) ; Utskrift av information
  (+ a b)) ; Retunerar summan av a och b
```
Output:
```
Adding 2 and 3
5
```

Djupdykning:
Debuggningsutskrift har funnits länge och är ett beprövat sätt att felsöka kod. Alternativa metoder som används idag inkluderar loggning och att använda en debugger. Implementationen av utskrift kan variera beroende på programmeringsspråk, men grundprincipen är densamma - att visa information under körning av programmet.

Se också:
- [The Art of Debugging](https://www.amazon.com/Art-Debugging-Indispensable-Techniques-Software/dp/1118912879) av Matt Telles and Andy Fundinger
- [Debugging in Clojure](https://clojure.org/guides/debugging) på Clojure.org dokumentationssidan