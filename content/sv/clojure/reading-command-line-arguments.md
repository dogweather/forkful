---
title:                "Clojure: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Varför
Att kunna läsa kommandoradsargument är en värdefull färdighet för varje programmerare. Det ger dig möjlighet att interagera direkt med dina program på ett snabbt och effektivt sätt.

##Hur man gör
För att läsa kommandoradsargument i Clojure kan du använda funktionen `clojure.core/command-line-args`. Här är ett exempel på hur du kan använda den:

```Clojure
(def args (command-line-args))
(println "De givna argumenten var: " args)
```

Om vi kör detta program med följande kommandoradsargument:

`$ clojure args-test.clj argument1 argument2`

Så kommer utmatningen att vara:

`De givna argumenten var: ["argument1" "argument2"]`

Som du kan se har variabeln `args` nu de givna argumenten som en lista. Du kan sedan använda dessa argument i ditt program för att exempelvis välja en specifik handling baserad på dem.

##Djupdykning
Det finns en mängd olika sätt att läsa kommandoradsargument i Clojure, och det beror på hur du vill hantera och använda dem i ditt program. En populär metod är att använda biblioteket `tools.cli`, vilket ger dig mer flexibilitet och möjlighet att definiera hur argumenten ska tolkas och användas.

Det finns också möjlighet att använda `clojure.core/env` för att läsa in miljövariabler från kommandoraden.

##Se även
- <https://clojuredocs.org/clojure.core/command-line-args>
- <https://github.com/clojure/tools.cli>
- <https://clojuredocs.org/clojure.core/env>