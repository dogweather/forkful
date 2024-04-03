---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:36.527678-07:00
description: 'Hoe: Clojure maakt gebruik van de Java interop-mogelijkheden om met
  datums om te gaan. Laten we onze mouwen opstropen en duiken erin.'
lastmod: '2024-03-13T22:44:50.432643-06:00'
model: gpt-4-0125-preview
summary: Clojure maakt gebruik van de Java interop-mogelijkheden om met datums om
  te gaan.
title: Twee datums vergelijken
weight: 27
---

## Hoe:
Clojure maakt gebruik van de Java interop-mogelijkheden om met datums om te gaan. Laten we onze mouwen opstropen en duiken erin:

```clojure
;; Importeer Java Date klasse
(import java.util.Date)

;; Maak twee datuminstanties
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; Wacht even
(def date2 (java.util.Date.))

;; Vergelijk de datums
(println (.before date1 date2)) ; waar, date1 is voor date2
(println (.after date1 date2))  ; onwaar, date1 is niet na date2
(println (.equals date1 date2)) ; onwaar, date1 is niet hetzelfde als date2
```

Een voorbeelduitvoer kan er zo uitzien, maar met verschillende tijdstempels:

```
waar
onwaar
onwaar
```

## Diepgaand
In het verleden gebruikten Clojure-ontwikkelaars vaak Java's `Date` voor datumoperaties, waarbij methoden werden aangeroepen met behulp van de puntoperator zoals eerder gezien. Alternatieven zijn onder meer `clj-time`, een Clojure-bibliotheek die Joda-Time omhult.

Een voorbeeld met `clj-time` ziet er als volgt uit:

```clojure
;; Voeg clj-time toe aan je projectafhankelijkheden
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; Maak twee datum-tijdinstanties
(def date-time1 (time/now))
(Thread/sleep 1000) ;; Wacht een seconde
(def date-time2 (time/now))

;; Vergelijk met behulp van clj-time functies
(println (time/before? date-time1 date-time2)) ; waar
(println (time/after? date-time1 date-time2))  ; onwaar
(println (time/equal? date-time1 date-time2))  ; onwaar
```

Clojure's standpunt over tijd is het benutten van Java's bibliotheken, terwijl clj-time integreert met Joda-Time voor een meer idiomatische Clojure-ervaring.

Sinds Java 8 is het `java.time` pakket—geïnspireerd door Joda-Time—de voorkeursmanier om met datums en tijden in Java om te gaan en, bij uitbreiding, in Clojure via interop. Verbeterd ontwerp en extra mogelijkheden zoals tijdzones maken `java.time` een robuuste keuze.

## Zie Ook
- [Clojure's Java Interop](https://clojure.org/reference/java_interop)
- [clj-time GitHub-repository](https://github.com/clj-time/clj-time)
- [Java Datum en Tijd API Gids](https://docs.oracle.com/javase/tutorial/datetime/)
