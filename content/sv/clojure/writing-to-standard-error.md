---
title:    "Clojure: Skriva till standardfel"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är ett användbart verktyg för att på ett enkelt och tydligt sätt visa felmeddelanden och andra viktiga utskrifter till användaren.

## Hur du gör det

Det första steget för att kunna skriva till standard error är att importera funktionen `clojure.core/print-err`. Sedan kan du enkelt skriva till standard error genom att lägga in en variabel eller ett uttryck inom parenteser.

```Clojure
(ns example.core
  (:require [clojure.core :refer [print-err]]))

(print-err "Detta är ett felmeddelande.")
;; => Detta är ett felmeddelande.
```

Du kan även kombinera flera variabler eller uttryck genom att använda funktionen `clojure.string/join`.

```Clojure
(print-err (str "Detta" "är" "ett" "felmeddelande."))
;; => Detta är ett felmeddelande.
```

## Djupdykning

Att skriva till standard error är användbart i olika situationer där du vill kommunicera viktiga meddelanden till användaren. Det kan vara felmeddelanden, debug-utskrifter eller bara viktig information som användaren behöver veta. Det är också ett bra sätt att skilja felmeddelanden från vanliga utskrifter, eftersom det skrivs ut i en annan färg i konsolen.

Det är också viktigt att notera att när du skriver till standard error så blockeras inte programmet, vilket betyder att programmet fortsätter att köra som vanligt. Det är en fördel i situationer där du vill fortsätta köra programmet även när ett fel uppstår.

## Se även

- [Officiell Clojure-dokumentation om standard error](https://clojure.org/reference/java_interop#_standard_error)
- [En bloggpost om användningen av standard error i Clojure](https://purelyfunctional.tv/article/how-to-handle-errors-in-clojure/)