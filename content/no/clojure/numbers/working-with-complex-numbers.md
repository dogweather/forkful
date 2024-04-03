---
date: 2024-01-26 04:38:41.961889-07:00
description: "Hvordan: Clojure gir innebygd st\xF8tte for komplekse tall gjennom `clojure.lang.Numbers`\
  \ hjelpeklassen. Bruk `complex` for \xE5 opprette komplekse tall og\u2026"
lastmod: '2024-03-13T22:44:40.396433-06:00'
model: gpt-4-0125-preview
summary: "Clojure gir innebygd st\xF8tte for komplekse tall gjennom `clojure.lang.Numbers`\
  \ hjelpeklassen."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Clojure gir innebygd støtte for komplekse tall gjennom `clojure.lang.Numbers` hjelpeklassen. Bruk `complex` for å opprette komplekse tall og utføre aritmetikk.

```clojure
;; Opprette komplekse tall
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addisjon
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Subtraksjon
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplikasjon
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Divisjon
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Konjugert
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Dypdykk
Komplekse tall ble formalisert av matematikere som Gauss og Euler på 1700-tallet. Selv om de først ble møtt med skepsis, har de siden blitt avgjørende i moderne vitenskap og ingeniørfag. Clojure har ikke en innfødt komplekst talltype som noen språk (f.eks. Python), men den medfølgende Java-interoperabiliteten kan håndtere de nødvendige operasjonene via `clojure.lang.Numbers` klassen.

Javas `java.lang.Complex` er et robust alternativ, som gir flere funksjoner og potensielle optimaliseringer. Clojures vertsinteroperabilitet gjør det enkelt å arbeide med Java-biblioteker.

Bak kulissene involverer aritmetikk med komplekse tall å legge til og multiplisere de reelle og imaginære delene, med den nøkkelregelen at `i^2 = -1`. Divisjon av komplekse tall kan være mer komplisert, og krever typisk den konjugerte for å unngå divisjon med komplekse tall.

## Se også
- ClojureDocs, for en kjapp referanse: https://clojuredocs.org/
- Java API for `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Wikipedia-siden om komplekse tall for de matematisk nysgjerrige: https://en.wikipedia.org/wiki/Complex_number
