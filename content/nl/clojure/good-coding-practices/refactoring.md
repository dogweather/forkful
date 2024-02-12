---
title:                "Refactoring"
date:                  2024-01-28T22:05:29.817543-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring is het proces van het herstructureren van bestaande computercodes zonder het externe gedrag ervan te veranderen, gericht op het verbeteren van niet-functionele attributen. Programmeurs refactoren om hun code schoner, efficiënter en makkelijker te onderhouden te maken, wat effectief de leesbaarheid verhoogt en de complexiteit van hun software vermindert.

## Hoe:

Refactoring in Clojure—dankzij de heldere syntaxis en functionele paradigma—kan ongelooflijk rechttoe rechtaan zijn. Laten we een veelvoorkomend scenario aanpakken: itereren over collecties. Je zou kunnen beginnen met een `for` lus, zoals dit:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Het aanroepen van `(old-way)` geeft ons 55, de som van 1 tot 10. Maar hé, we kunnen dit refactoren om meer Clojure-esk te zijn:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Deze gerefactorde `(new-way)` functie gebruikt threading macro's om het bereik direct naar `reduce` te leiden, waardoor overtollige complexiteit wordt weggesneden.

## Diepgaande Duik

De kunst van refactoring heeft zijn wortels in de vroege dagen van softwareontwikkeling maar kreeg echt tractie met het baanbrekende boek van Martin Fowler "Refactoring: Improving the Design of Existing Code" gepubliceerd in 1999. In Clojure leunt refactoring vaak op functionele programmeerprincipes, met een voorkeur voor pure functies en onveranderlijke datastructuren.

Alternatieven voor handmatige refactoring in Clojure kunnen het gebruik van tools zoals Cursive, een populaire IntelliJ IDEA plug-in, omvatten, die geautomatiseerde refactors specifiek voor Clojure biedt. Er is ook clj-refactor, een Emacs-pakket voor Clojure, dat een reeks refactoringfuncties levert.

Een specifieke uitdaging voor refactoring in Clojure is het omgaan met staat en bijwerkingen in een voornamelijk onveranderlijk en bijwerkingvrij paradigma. Zorgvuldig gebruik van atoms, refs, agents en transients zijn cruciaal in het behouden van zowel prestaties als correctheid tijdens refactorings.

## Zie Ook

- Martin Fowler's "Refactoring: Improving the Design of Existing Code" voor de fundamentele concepten.
- [Clojure Docs](https://clojuredocs.org/) voor specifieke voorbeelden van idiomatische Clojure-code.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) voor automatisering van refactoring in Emacs.
- [Cursive](https://cursive-ide.com/) voor IntelliJ-gebruikers die geautomatiseerde refactoringondersteuning zoeken.
- [Refactoring met Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Een lezing van de maker van Clojure die, hoewel niet specifiek over refactoring, inzicht biedt in de Clojure-filosofie die effectieve refactoringbeslissingen kan begeleiden.
