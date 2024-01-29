---
title:                "Debug-output afdrukken"
date:                  2024-01-28T22:04:25.365706-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Debug-output afdrukken is als het achterlaten van broodkruimels in je code: het toont het spoor van gegevens en logische stroom tijdens de uitvoering. Programmeurs gebruiken het om lastige bugs op te sporen en om te begrijpen of hun code zich gedraagt zoals verwacht.

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
