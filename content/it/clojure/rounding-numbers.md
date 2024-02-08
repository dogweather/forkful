---
title:                "Arrotondamento dei numeri"
aliases:
- it/clojure/rounding-numbers.md
date:                  2024-01-26T03:43:43.275396-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Arrotondare i numeri consiste nel modificare un numero per approssimarlo all'intero più vicino, o a una certa precisione decimale. Arrotondiamo i numeri per semplificarli in termini di leggibilità umana, ridurre il carico computazionale o soddisfare specifici requisiti numerici.

## Come fare:
In Clojure, utilizziamo principalmente `Math/round`, `Math/floor` e `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Per decimali specifici, moltiplichiamo, arrotondiamo e dividiamo:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Approfondimento
Prima dei linguaggi di programmazione attuali, l'arrotondamento era un processo manuale, pensate all'abaco o alla carta. Nella programmazione, è cruciale per la rappresentazione dei numeri a causa delle limitazioni di precisione dei numeri in virgola mobile.

Alternative per l'arrotondamento includono l'uso della classe `BigDecimal` per il controllo della precisione o librerie come `clojure.math.numeric-tower` per funzioni matematiche avanzate. `Math/round` di Clojure si basa su `Math.round`, `Math/floor` e `Math/ceil` di Java, il che significa che eredita le stesse sfumature relative ai tipi float e double.

Dal punto di vista dell'implementazione, quando si arrotonda in Clojure, ricorda che automaticamente utilizza la doppia precisione quando si occupa di decimali. Attenzione agli errori di arrotondamento!

## Vedi Anche
- API Matematiche di Clojure: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API Matematiche di Java: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Capire la Precisione dei Numeri in Virgola Mobile: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
