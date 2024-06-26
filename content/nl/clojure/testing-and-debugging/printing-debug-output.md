---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.365706-07:00
description: 'Hoe te: In Clojure print je vaak debug-output met behulp van `println`,
  `printf`, `pr`, of `prn`. Hier is hoe je wat debug-prints toevoegt.'
lastmod: '2024-03-13T22:44:50.422228-06:00'
model: gpt-4-0125-preview
summary: In Clojure print je vaak debug-output met behulp van `println`, `printf`,
  `pr`, of `prn`.
title: Debug-output afdrukken
weight: 33
---

## Hoe te:
In Clojure print je vaak debug-output met behulp van `println`, `printf`, `pr`, of `prn`. Hier is hoe je wat debug-prints toevoegt:

```Clojure
(defn add-and-print [a b]
  (println "Optellen:" a "en" b) ; Print de operatie
  (let [resultaat (+ a b)]
    (println "Resultaat:" resultaat)  ; Print het resultaat
    resultaat))                       ; Geeft het resultaat terug

(add-and-print 3 4)
```
Voorbeelduitvoer:
```
Optellen: 3 en 4
Resultaat: 7
```

Of, om waardes te debuggen in het midden van een threading macro:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; Print tussentijds resultaat
    (* 2))
```
Voorbeelduitvoer:
```
8
```

## Diepgaand:
Print-debugging heeft een lange geschiedenis, waarschijnlijk zo oud als programmering zelf. Het is eenvoudig: je voegt printopdrachten in waar je problemen vermoedt, draait de code en bekijkt de uitvoer.

Clojure's functies voor debug-printen lijken sterk op die in andere Lisp-talen, maar dan met de gebruikelijke functionele smaak. `println` en `prn` verschillen in dat de laatste gegevens schrijft op een manier die kan worden gelezen door de Clojure-lezer. `pprint` (mooi printen) van `clojure.pprint` kan worden gebruikt wanneer je een mooier formaat wilt.

Een specifiek hulpmiddel voor Clojure voor debuggen is `tap>`. Geïntroduceerd in Clojure 1.10, laat het toe om niet-blokkerende 'taps' in lopende code te doen zonder je code te moeten vervuilen met printfuncties.

Voor grotere of meer complexe projecten, overweeg een loggingbibliotheek zoals `clojure.tools.logging` of `timbre`.

## Zie Ook:
- [`clojure.tools.logging`](https://github.com/clojure/tools.logging) GitHub-repository
- [Timbre loggingbibliotheek](https://github.com/ptaoussanis/timbre) GitHub-repository
- [`clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) documentatie op ClojureDocs
