---
title:    "Clojure: Utskrift av felsökningsresultat"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en process som ofta involverar felsökning och felsökningsutdata kan vara en livräddare i dessa situationer. Genom att lägga till utskrift av debugmeddelanden i vår kod kan vi få en bättre förståelse för vad som händer under utförandet och identifiera eventuella problem i vårt program. Det kan också hjälpa oss att bekräfta att vår kod beter sig som vi förväntar oss.

## Hur man gör

Det finns flera sätt att skriva ut debugmeddelanden i Clojure. Ett enkelt sätt är att använda funktionen `println`, som tar emot ett eller flera argument och skriver ut dem på en ny rad. Till exempel:

```Clojure
(println "Hello" "world")
;; Output: Hello world
```

Om vi vill ha mer kontroll över formatet på vår utdata kan vi istället använda `println-str`, som returnerar en sträng med det formaterade utskriftsmeddelandet istället för att skriva ut det på en ny rad. Till exempel:

```Clojure
(println-str "Hello" "world")
;; Output: "Hello world"
```

Vi kan också använda funktionen `prn` för att skriva ut våra argument utan att lägga till en ny rad i slutet. Till exempel:

```Clojure
(prn "Hello world")
;; Output: "Hello world"
```

## Djupdykning

En annan användbar funktion för att skriva ut debugmeddelanden är `dbg`, som kommer från biblioteket clojure.tools.trace. Denna funktion tar emot ett värde och skriver ut det tillsammans med information om vilken funktion och fil det kommer ifrån. Till exempel:

```Clojure
(require '[clojure.tools.trace :refer [dbg]])
(defn add [a b] (+ a b))
(dbg (add 2 3))
;; Output: TRACE add
;; 5
```

Vi kan också använda `dbg` tillsammans med wildcard ("*") för att få ut all information om en funktion. Till exempel:

```Clojure
(require '[clojure.tools.trace :refer [dbg]])
(dbg add*)
```

Vi kan också använda `clojure.tools.logging`, som tillhandahåller en wrap-primitiv för att logga information istället för att skriva ut den i konsolen. Till exempel:

```Clojure
(require '[clojure.tools.logging :refer [info]])
(defn add [a b] (info "Adding" a "and" b) (+ a b))
(add 2 3)
;; Output: [main] INFO user - Adding 2 and 3
;; 5
```

## Se även

- Clojure-officiell dokumentation: https://clojure.org/
- Karrdahl, P., Hultin, L., Berg, A. (2017). Clojure för Scala-programmeraren: https://www.liber.se/efter-amn/utveckling/clojure-for-scala-programmeraren.html