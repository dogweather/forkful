---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:55.794499-07:00
description: 'Hoe: Om de lengte van een string in Clojure te krijgen, gebruik je de
  `count` functie.'
lastmod: '2024-03-13T22:44:50.409514-06:00'
model: gpt-4-0125-preview
summary: Om de lengte van een string in Clojure te krijgen, gebruik je de `count`
  functie.
title: De lengte van een string vinden
weight: 7
---

## Hoe:
Om de lengte van een string in Clojure te krijgen, gebruik je de `count` functie:

```clojure
(count "Hallo, Wereld!") ;=> 13
```

Dit betekent dat "Hallo, Wereld!" 13 karakters heeft.

## Diepgaande kijk
De `count` functie is in Clojure de voor de hand liggende keuze voor het vinden van het aantal items in een verzameling, en strings vormen hier geen uitzondering op aangezien ze behandeld kunnen worden als een reeks van karakters. Historisch gezien maakt `count` al deel uit van Clojure sinds de vroege versies, wat reflecteert op de roots in Lisp waar lengteoperaties gebruikelijk zijn op lijsten.

Een alternatief voor `count` zou het gebruik van Java interop kunnen zijn omdat Clojure op de JVM draait:

```clojure
(.length "Hallo, Wereld!") ;=> 13
```

Dit roept de `.length` methode aan van Java's String klasse. Hoewel dit alternatief bestaat, is het gebruik van `count` meer idiomatic Clojure.

Het is het vermelden waard dat `count` een O(1) operatie is voor strings, wat betekent dat het een constante hoeveelheid tijd kost ongeacht de lengte van de string, aangezien de metadata van de stringlengte gecachet wordt.

## Zie Ook
- Officiële Clojure documentatie over `count`: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
