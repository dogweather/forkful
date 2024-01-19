---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debug-utdata innebär att programmerare skapar meddelanden under körning av programmet för att hjälpa till att felsöka det. Vi gör det för att förstå hur vårt program fungerar i realtid och för att upptäcka eventuella fel.

## Så här gör du:

Det är enkelt att skriva ut debug-information i Clojure. Lösningen är standardfunktionen `println`. Kolla här:

```Clojure
(defn hello-world []
  (let [message "Hello, World!"]
    (println "Debug: " message)
    (println message)))

(hello-world)
```

Utdata kommer att vara:

```
Debug: Hello, World!
Hello, World!
```

## Djupdykning:

Skriva ut debug-utdata är en grundläggande men viktig del av att vara en programmerare. Detta koncept återfinns i nästan alla programmeringsspråk, inte bara Clojure.

Att ha alternativ ger oss mer kraft. I Clojure kan du även använda verktyget `clojure.tools.logging` för mer sofistikerad loggning. Med `clojure.tools.logging` kan du till exempel välja att bara skriva ut meddelanden med en viss nivå av allvar.

Den typ av utskrift vi har pratat om skriver till standard output. Men vanligtvis skriver verktyg som `clojure.tools.logging` också till en loggfil.

## Se också:

För mer information om att skriva ut debug-utdata i Clojure, se följande resurser:

1. [Official Clojure Documentation](https://clojure.org/guides/getting_started)
2. [Clojure Tools Logging](https://github.com/clojure/tools.logging)
3. [Debugging in Clojure](https://clojuredocs.org/clojure.repl/pst)