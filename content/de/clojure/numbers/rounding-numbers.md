---
date: 2024-01-26 03:43:30.134244-07:00
description: "Wie: In Clojure verwenden wir haupts\xE4chlich `Math/round`, `Math/floor`\
  \ und `Math/ceil`."
lastmod: '2024-03-13T22:44:53.414789-06:00'
model: gpt-4-0125-preview
summary: "In Clojure verwenden wir haupts\xE4chlich `Math/round`, `Math/floor` und\
  \ `Math/ceil`."
title: Zahlen runden
weight: 13
---

## Wie:
In Clojure verwenden wir hauptsächlich `Math/round`, `Math/floor` und `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Für spezifische Dezimalstellen multiplizieren, runden und teilen wir:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Tiefergehend
Bevor es ausgeklügelte Programmiersprachen gab, war das Runden ein manueller Prozess, denke an den Abakus oder Papier. In der Programmierung ist es aufgrund von Präzisionsbegrenzungen bei Fließkommazahlen für die Zahlendarstellung entscheidend.

Alternativen zum Runden beinhalten die Verwendung der `BigDecimal`-Klasse für Präzisionskontrolle oder Bibliotheken wie `clojure.math.numeric-tower` für fortgeschrittene mathematische Funktionen. Clojures `Math/round` basiert auf Javas `Math.round`, `Math/floor` und `Math/ceil` Funktionen, was bedeutet, dass es die gleichen Float- und Double-Nuancen erbt.

Implementierungstechnisch, wenn in Clojure gerundet wird, denken Sie daran, dass automatisch eine doppelte Genauigkeit bei Dezimalzahlen verwendet wird. Passen Sie auf Rundungsfehler auf!

## Siehe auch
- Clojure Math-API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math-API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Verständnis der Genauigkeit von Fließkommazahlen: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
