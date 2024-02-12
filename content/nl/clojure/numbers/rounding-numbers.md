---
title:                "Afronden van getallen"
aliases: - /nl/clojure/rounding-numbers.md
date:                  2024-01-28T22:06:42.589811-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Getallen afronden gaat over het aanpassen van een getal naar het dichtstbijzijnde geheel of naar een bepaalde decimale precisie. We ronden getallen af om ze te vereenvoudigen voor menselijke leesbaarheid, rekenlast te verminderen, of aan specifieke numerieke eisen te voldoen.

## Hoe:
In Clojure gebruiken we voornamelijk `Math/round`, `Math/floor`, en `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Voor specifieke decimalen vermenigvuldigen, ronden en delen we:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Diepe Duik
Voor de komst van geavanceerde programmeertalen was afronden een handmatig proces, denk maar aan een abacus of papier. In programmering is het cruciaal voor de getallenrepresentatie vanwege de precisielimieten van drijvende-kommagetallen.

Alternatieven voor afronden omvatten het gebruik van de `BigDecimal` klasse voor precisiecontrole of bibliotheken zoals `clojure.math.numeric-tower` voor geavanceerde wiskundige functies. Clojures `Math/round` leunt op Java's `Math.round`, `Math/floor`, en `Math/ceil` functies, wat betekent dat het dezelfde nuances van float en double erft.

Qua implementatie, wanneer je in Clojure afrondt, onthoud dan dat het automatisch dubbele precisie gebruikt bij het omgaan met decimalen. Pas op voor afrondingsfouten!

## Zie Ook
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Begrijpen van Drijvende-kommagetallen Precisie: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
