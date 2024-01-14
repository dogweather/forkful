---
title:    "Clojure: Generering av slumpmässiga tal"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del inom datorskapande och programmering. Det kan användas för att testa kod, skapa spel eller simulera slumpmässiga händelser.

## Hur gör man

För att generera slumpmässiga nummer i Clojure kan du använda funktionen `rand` tillsammans med `rand-int` för att få ett heltal. Här är ett exempel på hur du kan generera 5 slumpmässiga nummer mellan 1 och 10 och skriva ut dem:

```Clojure
(defn rand-numbers []
  (println "Slumpmässiga nummer:")
  (dotimes [n 5]
    (let [number (rand-int 10)]
      (print (str number " "))))
  (println))
```

Output:

```
Slumpmässiga nummer:
9 2 6 5 1
```

Det finns också möjlighet att begränsa antalet decimaler genom att använda `rand` med funktionen `decimal`, som i följande exempel:

```Clojure
(defn rand-decimals []
  (println "Slumpmässiga decimaler:")
  (dotimes [n 5]
    (let [decimal (rand 1.0)]
      (print (str (decimal 0) " "))))
  (println))
```

Output:

```
Slumpmässiga decimaler:
0.8 0.7 0.8 0.1 0.7
```

## Djupdykning

Bakom kulisserna så använder sig Clojure av Java's `java.util.Random` för att generera slumpmässiga nummer. Detta innebär att det finns ett större utbud av funktioner för att manipulera och styra slumpmässiga nummer, inklusive sätt att kontrollera den så kallade "seed" som används för att börja slumpmässigheten.

Det är också viktigt att vara medveten om att slumpmässiga nummer i datavetenskap egentligen inte är helt slumpmässiga, utan följer ett förutbestämt mönster som brukar kallas för en "pseudorandom" sekvens. Detta innebär att om samma "seed" används så kommer samma slumpmässiga sekvens att genereras. Detta är dock ofta tillräckligt för de flesta användningsområden.

## Se även

- [Clojure documentationsida för "rand"](https://clojuredocs.org/clojure.core/rand)
- [Java dokumentationsida för "java.util.Random"](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html) 
- [En djupare förklaring av pseudorandom sekvenser](https://www.techopedia.com/definition/27809/pseudorandom)