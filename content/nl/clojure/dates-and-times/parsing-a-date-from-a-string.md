---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:46.882216-07:00
description: "Het parseren van een datum uit een string betekent het converteren van\
  \ tekst met een datum die door mensen gelezen kan worden, naar een formaat dat de\u2026"
lastmod: '2024-03-13T22:44:50.429734-06:00'
model: gpt-4-0125-preview
summary: "Het parseren van een datum uit een string betekent het converteren van tekst\
  \ met een datum die door mensen gelezen kan worden, naar een formaat dat de\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het parseren van een datum uit een string betekent het converteren van tekst met een datum die door mensen gelezen kan worden, naar een formaat dat de computer begrijpt. Programmeurs doen dit omdat computers de voorkeur geven aan datums als getallen voor sorteren, opslaan of manipuleren.

## Hoe te:

Clojure leunt voor het parsen van datums op Java, dus we zullen hier `java.time.LocalDate` gebruiken:

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date "yyyy-MM-dd" date-str))

(println (parse-date "2023-04-05"))
```

Output: 

```
#object[java.time.LocalDate 0x4b121a5e "2023-04-05"]
```

Hier is `java-time` een Clojure-bibliotheek die de `java.time` API's omvat. Het is idiomatischer Clojure dan ruwe Java interop.

## Diepere duik

Clojure, geboren in 2007, is een moderne Lisp die draait op de JVM. Het biedt interoperabiliteit met Java, inclusief datumafhandeling. Voor `java.time` (geïntroduceerd in Java 8), gebruikte Java `java.util.Date` en `java.text.SimpleDateFormat`, houterig en minder thread-safe.

`clj-time`, een Joda-Time wrapper, was populair voor Clojure voor `java-time`, maar Joda-Time wordt nu als verouderd beschouwd. Tegenwoordig is `java-time` de go-to aangezien het rond het `java.time` pakket wikkelt, dat veruit superieur is en standaard onveranderlijk.

Er zijn ook pure Clojure-bibliotheken, zoals `tick`, maar die bouwen ook bovenop Java's `java.time` om praktische redenen. Het onderliggende `java.time`-pakket gebruikt het ISO kalendersysteem maar ondersteunt ook andere systemen. Zo'n flexibiliteit betekent dat Clojure-programma's niet alleen JVM-vriendelijk zijn, maar ook internationaal klaar.

## Zie Ook

- [Clojure Documentatie](https://clojure.org/)
- [java-time bibliotheek](https://github.com/dm3/clojure.java-time)
- [Oudere clj-time bibliotheek](https://github.com/clj-time/clj-time)
- [Java SE Datum Tijd](https://docs.oracle.com/javase/tutorial/datetime/)

Blijf verkennen en blij coderen!
