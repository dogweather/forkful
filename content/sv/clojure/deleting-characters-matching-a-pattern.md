---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ta bort tecken efter ett mönster innebär att du identifierar och tar bort specifika tecken från en sträng baserat på ett specifikt mönster. Detta är viktigt för programmerare, eftersom det hjälper till att rensa och format om data effektivt.

## Hur man gör:

För att ta bort tecken som matchar ett mönster i Clojure kan vi använda `clojure.string/replace`-funktionen. Till exempel:

```clojure
(require '[clojure.string :as str])

(defn delete-recurring-letters [s]
  (str/replace s #"(.)\1+" "$1"))

(println (delete-recurring-letters "aaaabbbbcccc")) 
; output: abcc
```
I kolven ovan ersätter vi tecken som återkommer mer än en gång med det ursprungliga tecken.

## Fördjupning:

Funktionen `clojure.string/replace` är så kraftfull eftersom den använder regelbundna uttryck (regex) för att identifiera mönster, vilket är en del av många programmeringsspråks historia. Emellertid fanns det andra metoder för att ta bort tecken som matchar ett mönster innan regex blev gängse, som att använda öglor. 

Ett alternativ till `str/replace` är att använda `clojure.string/replace-first` vilket endast ersätter första matchningen istället för alla matchningar.

Funktionens genomförande involverar kompilering av regex-mönstret, utför sedan en matchning av mönstret mot strängen, och infogar därefter den givna ersättningen varje gång en matchning hittas.

## Läs mer:

- Clojure-string manual: https://clojuredocs.org/clojure.string
- Regex Wikipedia: https://en.wikipedia.org/wiki/Regular_expression
- En bok om Clojure: https://www.braveclojure.com/clojure-for-the-brave-and-true/