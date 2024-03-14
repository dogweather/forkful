---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:48.922649-07:00
description: "Code in functies opdelen gaat over het inpakken van codeblokken die\
  \ specifieke taken uitvoeren. Dit maakt je code schoon, makkelijker te onderhouden,\
  \ en\u2026"
lastmod: '2024-03-13T22:44:50.425522-06:00'
model: gpt-4-0125-preview
summary: "Code in functies opdelen gaat over het inpakken van codeblokken die specifieke\
  \ taken uitvoeren. Dit maakt je code schoon, makkelijker te onderhouden, en\u2026"
title: Code organiseren in functies
---

{{< edit_this_page >}}

## Wat & Waarom?

Code in functies opdelen gaat over het inpakken van codeblokken die specifieke taken uitvoeren. Dit maakt je code schoon, makkelijker te onderhouden, en eenvoudig te lezen voor andere ontwikkelaars.

## Hoe te:

Clojure-functies worden gedefinieerd met `defn`, gevolgd door een naam, parameters en een lichaam. Hier is een snel voorbeeld.

```Clojure
(defn groet [naam]
  (str "Hallo, " naam "!"))

(groet "Alex") ; => "Hallo, Alex!"
```

Stel nu dat we de oppervlakte van een rechthoek willen berekenen. In plaats van alles samen te proppen, scheiden we het in twee functies:

```Clojure
(defn oppervlakte [lengte breedte]
  (* lengte breedte))

(defn print-oppervlakte [lengte breedte]
  (println "De oppervlakte is:" (oppervlakte lengte breedte)))

(print-oppervlakte 3 4) ; => De oppervlakte is: 12
```

## Diepere Duik

Heel lang geleden zouden programmeurs gewoon al hun logica in een enkel blok proppen. Het was lelijk. Toen kwam gestructureerd programmeren langs, en functies werden een ding. In Clojure is elke functie first-class—je kunt ze rondgooien zoals elke andere waarde.

Alternatieven? Sommige mensen zouden misschien rommelen met multi-methoden of hogere-orde functies, maar die zijn slechts kruiden in de functiestoofpot.

Allemaal in de details van een functie: ze zijn onveranderlijk in Clojure, waardoor de kans op bijeffecten minder waarschijnlijk wordt. Ze leunen zwaar op recursie in plaats van typische lussen, wat goed samengaat met de functionele paradigma's van de taal.

## Zie Ook

- Clojure's eigen gids: https://clojure.org/guides/learn/functions
- Basisprincipes van functioneel programmeren: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickey's Voordrachten: https://changelog.com/posts/rich-hickeys-greatest-hits - voor inzicht in de filosofie van Clojure.
