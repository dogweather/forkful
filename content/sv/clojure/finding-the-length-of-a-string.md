---
title:                "Att hitta längden på en sträng"
html_title:           "Clojure: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför 

Att hitta längden på en sträng är en grundläggande och nödvändig uppgift inom programmering. Genom att förstå hur man kan få längden på en sträng i Clojure, kan du göra det enklare att hantera data och skapa mer effektiva och eleganta program.

## Hur 

För att få längden på en sträng i Clojure kan du använda funktionen `count`. Den tar en sträng som argument och returnerar längden som ett heltal.

```Clojure 
(count "Hej världen") 
; Output: 11 
```

Detta användbara verktyg kan också användas på andra datatyper, som vektorer och listor.

```Clojure 
(count [1 2 3 4 5]) 
; Output: 5 
```

En annan metod är att använda `(str "sträng")`. Detta konverterar en sträng till en sekvens som du sedan kan räkna med funktionen `count`.

```Clojure 
(count (str "Hej världen")) 
; Output: 11 
```

Du kan också använda `seq` för att förvandla en sträng till en sekvens av tecken, som sedan kan räknas på samma sätt.

```Clojure 
(count (seq "Hej världen")) 
; Output: 11 
``` 

## Djupdykning 

När du använder `count`-funktionen, måste du vara medveten om att den returnerar antalet tecken och inte antalet bytes eller symboler. Det betyder att om din sträng innehåller multibyte-tecken, som till exempel ä, så kommer `count` att räkna utfärders byte storlek. Det här beteendet kan förändras genom att använda funktionen `counted?`, vilket returnerar sant om längden på sekvensen har räknats. Du kan också använda `nippy/count` som hanterar både bokstäver och bytes på ett bättre sätt, om det är ett problem för ditt program.

## Se även 

- [Clojure.org - Strings](https://clojure.org/guides/strings)
- [ClojureDocs - count](https://clojuredocs.org/clojure.core/count) 
- [ClojureDocs - counted?](https://clojuredocs.org/clojure.core/counted_q)
- [ClojureDocs - str](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs - seq](https://clojuredocs.org/clojure.core/seq)
- [ClojureDocs - nippy/count](https://clojuredocs.org/nippy/count)