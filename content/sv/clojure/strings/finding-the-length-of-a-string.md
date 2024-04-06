---
date: 2024-01-20 17:47:10.480137-07:00
description: "Hur man g\xF6r: I Clojure, som i m\xE5nga funktionella spr\xE5k, \xE4\
  r `count` en grundl\xE4ggande funktion som anv\xE4nds f\xF6r att hitta antalet element\
  \ i en samling.\u2026"
lastmod: '2024-04-05T21:53:38.844464-06:00'
model: gpt-4-1106-preview
summary: "I Clojure, som i m\xE5nga funktionella spr\xE5k, \xE4r `count` en grundl\xE4\
  ggande funktion som anv\xE4nds f\xF6r att hitta antalet element i en samling."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Hur man gör:
```Clojure
;; Använd `count` för att hitta längden på en sträng
(count "Hej, Sverige!")
;; => 13

;; Det fungerar också på listor och andra samlingar
(count [1 2 3 4 5])
;; => 5
```

## Djupdykning
I Clojure, som i många funktionella språk, är `count` en grundläggande funktion som används för att hitta antalet element i en samling. Historiskt sett har det alltid varit viktigt att veta längden på en datastruktur för att organisera och hantera information effektivt. Alternativ till `count` inkluderar att använda `length` i andra språk eller att iterera genom en sträng och räkna tecken, vilket är en ineffektiv metod i Clojure. Implementationen av `count` är optimerad för olika samlingstyper; för en sträng är det en direkt operation, men för länkade listor måste hela listan genomgås för att få fram en längd.

## Se även
- Clojure-dokumentationen om `count`: [https://clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- En diskussion om att hantera strängar i Clojure: [https://clojure.org/guides/faq#string_funcs](https://clojure.org/guides/faq#string_funcs)
- Artikel om funktionella språk och datastrukturer: [https://www.braveclojure.com/functional-programming/](https://www.braveclojure.com/functional-programming/)
