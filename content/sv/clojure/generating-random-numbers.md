---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:43.448869-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är processen att skapa tal som inte kan förutsägas. Programmerare använder det för spel, simulationer, säkerhet, och andra ställen där oförutsägbarhet är viktig.

## Så här gör du:
```clojure
;; Importera random namespace
(require '[clojure.core :as rand])

;; Skapa ett slumpmässigt heltal mellan 0 och 99
(rand/rand-int 100)
;; Exempel på utdata: 42

;; Skapa ett slumpmässigt flyttal mellan 0.0 och 1.0
(rand/rand)
;; Exempel på utdata: 0.7093

;; Slumpmässiga tal i en sekvens, exempelvis 5 stycken
(take 5 (repeatedly rand/rand-int 100))
;; Exempel på utdata: (73 58 22 89 2)
```

## Deep Dive
Slumptalsgeneratorer började som fysiska maskiner, men nu är digitala. Clojure använder Java:s `java.util.Random` klass som grund för sitt slumptalsbibliotek, fast med en funktionell twist. Man kan också använda andra bibliotek, som test.check, för mer komplex slumpgenerering.

Implementationsdetaljer är viktiga eftersom dåliga generatorer ger förutsägbara resultat, vilket är dåligt för säkerhet och spel. Clojure försöker därför använda robusta metoder för slumpalstring, samtidigt som det erbjuder enkelhet i användning.

## Se även:
- Clojure's random-funktionsdokumentation: https://clojuredocs.org/clojure.core/rand
- Java’s `java.util.Random` klassdokument: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- The `test.check` bibliotek för generativa tester: https://github.com/clojure/test.check
