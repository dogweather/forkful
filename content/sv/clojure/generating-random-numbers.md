---
title:                "Clojure: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Generering av slumpmässiga nummer är en viktig del av många programmeringsprojekt, oavsett om det handlar om spel, statistik eller testning av algoritmer. Genom att använda slumpmässiga nummer kan man skapa varierande och oförutsägbara resultat som gör programmet mer dynamiskt och användbart.

## Hur man gör

För att generera slumpmässiga nummer i Clojure, kan man använda funktionen "rand". Den här funktionen tar emot ett argument som specificerar vilken typ av nummer som ska genereras, till exempel en heltals- eller decimalvärde. Här nedan finns ett exempel på hur man kan använda "rand" för att skapa en lista med 5 slumpmässiga decimaltal mellan 0 och 1:

```Clojure
(let [numbers (repeatedly 5 #(rand))]
        (println numbers))
```

Output:

```Clojure
(0.8009568371398033 0.16302767434520087 0.4472026283181741 0.26524956201766594 0.8891182948462235)
```

Man kan också ange ett intervall för slumpmässiga heltal genom att använda "rand-int" funktionen. Nedan visas ett exempel på hur man kan generera tre slumpmässiga heltal mellan 1 och 10:

```Clojure
(let [numbers (repeatedly 3 #(rand-int 10))]
        (println numbers))
```

Output:

```Clojure
(3 7 9)
```

## Deep Dive

Clojure erbjuder också flera andra funktioner för att generera slumpmässiga nummer, som "shuffle" och "repeatedly". Dessutom kan man använda "seed" funktionen för att specificera en startpunkt för de slumpmässiga numren, vilket kan vara användbart vid testning av algoritmer.

En annan intressant aspekt av slumpmässiga nummer är att de inte är helt slumpmässiga, utan genereras från en given startpunkt med hjälp av en algoritm. Detta innebär att samma startpunkt kommer att generera samma serie av slumpmässiga nummer varje gång. Om man vill ha verkligt slumpmässiga nummer, kan man använda en extern slumpgenerator som till exempel "java.util.Random".

## Se också

- [Clojure's rand function documentation](https://clojuredocs.org/clojure.core/rand)
- [Random numbers in Clojure: a gentle introduction](https://lispcast.com/random-numbers-in-clojure-beginner/)
- [Generating Random Numbers in Clojure](https://blog.klipse.tech/clojure/2016/08/16/random-numbers-clojure.html) (på engelska)