---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:11.251125-07:00
description: "Complexe getallen breiden de re\xEBle getallen uit met een extra deel,\
  \ de imaginaire eenheid 'i'. Programmeurs gebruiken ze in verschillende domeinen,\u2026"
lastmod: '2024-03-13T22:44:50.413037-06:00'
model: gpt-4-0125-preview
summary: "Complexe getallen breiden de re\xEBle getallen uit met een extra deel, de\
  \ imaginaire eenheid 'i'. Programmeurs gebruiken ze in verschillende domeinen,\u2026"
title: Werken met complexe getallen
weight: 14
---

## Wat & Waarom?
Complexe getallen breiden de reële getallen uit met een extra deel, de imaginaire eenheid 'i'. Programmeurs gebruiken ze in verschillende domeinen, waaronder signaalverwerking, elektromagnetische theorie en fractals, waar berekeningen met de wortel van een negatief getal routine zijn.

## Hoe:
Clojure biedt ingebouwde ondersteuning voor complexe getallen via de utility-klasse `clojure.lang.Numbers`. Gebruik `complex` om complexe getallen te creëren en om rekenkundige bewerkingen uit te voeren.

```clojure
;; Creëren van complexe getallen
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Optellen
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Aftrekken
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Vermenigvuldigen
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Delen
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Conjugeren
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Diepere Duik
Complexe getallen werden geformaliseerd door wiskundigen als Gauss en Euler in de 18e eeuw. Hoewel ze aanvankelijk met scepsis werden ontvangen, zijn ze sindsdien cruciaal geworden in de moderne wetenschap en techniek. Clojure heeft geen native complex getaltype zoals sommige talen (bijv. Python), maar de bijgeleverde Java-interoperabiliteit kan de noodzakelijke bewerkingen aan via de klasse `clojure.lang.Numbers`.

Java's `java.lang.Complex` is een robuust alternatief, dat meer functies en potentiële optimalisaties biedt. Clojure’s host-interoperabiliteit maakt het eenvoudig om met Java-bibliotheken te werken.

Achter de schermen omvat de rekenkunde met complexe getallen het optellen en vermenigvuldigen van de reële en imaginaire delen, met als sleutelregel dat `i^2 = -1`. De deling van complexe getallen kan meer ingewikkeld zijn en vereist typisch het conjugeren om deling door complexe getallen te voorkomen.

## Zie Ook
- De ClojureDocs, voor een snelle referentie: https://clojuredocs.org/
- De Java API voor `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- De Wikipedia-pagina over complexe getallen voor wiskundig nieuwsgierigen: https://nl.wikipedia.org/wiki/Complex_getal
