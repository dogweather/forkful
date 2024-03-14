---
date: 2024-01-20 17:30:43.527734-07:00
description: "Kalkulering av fremtidige eller tidligere datoer handler om \xE5 legge\
  \ til eller trekke fra tidsenheter p\xE5 en startdato. Programmere bruker dette\
  \ for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.415150-06:00'
model: gpt-4-1106-preview
summary: "Kalkulering av fremtidige eller tidligere datoer handler om \xE5 legge til\
  \ eller trekke fra tidsenheter p\xE5 en startdato. Programmere bruker dette for\
  \ \xE5\u2026"
title: Beregning av en dato i fremtiden eller fortiden
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kalkulering av fremtidige eller tidligere datoer handler om å legge til eller trekke fra tidsenheter på en startdato. Programmere bruker dette for å håndtere frister, planlegge eventer og beregne tidsforskjeller.

## Hvordan Gjøre Det:
```clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

;; Beregner en dato 10 dager fra nå
(let [today (t/now)
      future-date (t/plus today (t/days 10))]
  (c/to-string future-date))
;;=> "2023-04-10T14:59:59.999Z"

;; Beregner en dato 5 år tilbake i tid
(let [today (t/now)
      past-date (t/minus today (t/years 5))]
  (c/to-string past-date))
;;=> "2018-03-31T14:59:59.999Z"
```

## Dypdykk
Beregner av datoer har vært nyttig siden starten av programmering. På den tiden var det vanlig å gjøre alt fra grunnen av. Nå har vi biblioteker som `clj-time`, som bygger på Jodas `java.time` biblioteket. I Clojure gir `clj-time` enklere, Clojure-vennlig syntaks for datohåndtering og operasjoner som å legge til eller trekke fra tidsperioder.

Alternativer til `clj-time` inkluderer innebygde funksjoner i `java.time`, eller for eldre prosjekter, klasser som `java.util.Date` og `java.util.Calendar`. Men disse kan være mer klossete og verbøse.

Et viktig detalj å huske når du jobber med tid og datoer er tidszoner. Alltid vurdere tidssonekonsekvenser, spesielt når du planlegger noe over forskjellige geografiske regioner.

## Se Også
- clj-time GitHub side: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
