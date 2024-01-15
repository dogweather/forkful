---
title:                "Generering av slumpmässiga nummer"
html_title:           "Clojure: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför

Att generera slumpmässiga nummer är en viktig del av många programmeringsprojekt. Det kan användas för att skapa variation och slumpmässighet i spel, simuleringsprogram, och många andra applikationer.

# Hur man gör det

```Clojure
;; För att generera ett slumpmässigt heltal från 0 till 10:
(rand-int 10)

;; För att generera ett slumpmässigt decimaltal från 0 till 1:
(rand)

;; För att generera ett slumpmässigt heltal från 1 till 100:
(+ (rand-int 100) 1)

;; För att generera ett slumpmässigt heltal från 5 till 10:
(+ (rand-int 5) 5)

;; För att generera ett slumpmässigt decimaltal från -10 till 10:
(+(rand 20) -10)
```

**Exempelutskrift:**

5
0.5140587545049382
64
6
-8.239453394421031

# Djupdykning

Det finns flera olika funktioner i Clojure för att generera slumpmässiga nummer, varav de vanligaste är "rand-int" och "rand". Rand-int genererar ett heltal, medan rand genererar ett decimaltal mellan 0 och 1.

För att få större kontroll över vilka tal som genereras, kan man använda funktionen "+" för att lägga till eller subtrahera från det genererade talet. Detta blir särskilt användbart om man vill ha heltal i ett visst intervall, istället för endast från 0 till en viss begränsning.

# Se även

- [Clojure.org - Slumpmässiga nummer](https://clojure.org/api/cheatsheet#random)
- [Clojure for the Brave and True - Randomness](https://www.braveclojure.com/core-functions-in-depth/#Randomness)