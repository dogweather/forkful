---
title:    "Clojure: Läsning av kommandoradsargument"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa kommandoargument i Clojure kan vara en användbar förmåga när du skriver program som behöver ta emot inputs från användaren. Genom att kunna läsa kommandoargument kan du enkelt få tillgång till olika värden och anpassa ditt program efter det.

## Hur man gör det

För att läsa kommandoargument i Clojure använder man sig av funktionen `command-line-args`. Den returnerar en lista med alla kommandoargument som skickats till programmet. Låt oss titta på ett litet exempel:

```Clojure
(defn print-args []
   (let [args (command-line-args)]
      (println "Antal argument:" (count args))
      (doseq [arg args]
         (println "Argument:" arg))))
```

Om vi kör detta program med kommandoargumenten `Clojure är roligt`, kommer följande output att visas:

```
Antal argument: 3
Argument: Clojure
Argument: är
Argument: roligt
```

Det är viktigt att notera att `command-line-args` returnerar en lista av strängar, så om du behöver använda argumenten som andra datatyper måste du konvertera dem.

## Djupdykning

Om du behöver mer kontroll över hur dina kommandoargument hanteras kan du använda dig av biblioteket Command Line Args. Det ger dig möjligheten att definiera flaggor och specificera vilken datatyp argumenten ska ha.

Till exempel kan vi använda biblioteket för att definiera ett kommando som tar emot en flagga `--name` med ett efterföljande värde av typen `string`:

```Clojure
(ns my-app.core
  (:require [clj-picocli.core :refer [command option]]))

(command (fn [args]
  (println "Hej," (:name args)))
  :name "say-hello"
  :options [ 
    (option :name "--name" :args 1 :type String)]))
```

Nu kan vi köra programmet med kommandot `lein run say-hello --name Adam` och få utskriften `Hej, Adam`.

## Se även

- [ClojureDocs Kommandoargument](https://clojuredocs.org/clojure.core/command-line-args)
- [Command Line Args biblioteket](https://github.com/ppikula/clj-picocli)