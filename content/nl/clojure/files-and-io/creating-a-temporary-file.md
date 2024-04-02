---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:05.915060-07:00
description: "Een tijdelijk bestand maken is het proces van het cre\xEBren van een\
  \ kortstondig bestand voor tussentijdse gegevensopslag. Programmeurs gebruiken ze\
  \ voor\u2026"
lastmod: '2024-03-13T22:44:50.439444-06:00'
model: gpt-4-0125-preview
summary: "Een tijdelijk bestand maken is het proces van het cre\xEBren van een kortstondig\
  \ bestand voor tussentijdse gegevensopslag. Programmeurs gebruiken ze voor\u2026"
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Wat & Waarom?
Een tijdelijk bestand maken is het proces van het creëren van een kortstondig bestand voor tussentijdse gegevensopslag. Programmeurs gebruiken ze voor dingen zoals caching, gegevensverwerking, of wanneer het beter is om de permanente opslag niet te belasten.

## Hoe:
Clojure maakt het simpel. De `clojure.java.io` bibliotheek staat voor je klaar.

```Clojure
(require '[clojure.java.io :as io])

; Een tijdelijk bestand maken
(def temp-bestand (io/file (io/create-temp-file "prefix-" ".txt")))

; Het tijdelijk bestand gebruiken
(spit temp-bestand "Tijdelijke data is tijdelijk")

; Inhoud controleren
(println (slurp temp-bestand)) ; => "Tijdelijke data is tijdelijk"

; Opruimen door het tijdelijk bestand te verwijderen als je klaar bent
(.delete temp-bestand)
```

Niets blijft voor altijd. Onze tijdelijke gegevens rusten nu in vrede.

## Diepere duik
Het concept van tijdelijke bestanden bestaat al sinds de vroege dagen van computing, voornamelijk om het gebruik van beperkte primaire opslag te vermijden. Het is als een digitale huurruimte.

Clojure leunt hier op de schouders van Java, gebruikmakend van de mogelijkheden van Java's `File` klasse. Hoewel je direct in de jungle van Java zou kunnen duiken, pakt Clojure het netjes in.

Alternatieven? Zeker. Temp mappen zijn ook een optie. Maar dat is een ander verhaal, en ook daar heeft Clojure een oplossing voor (zie `create-temp-dir`).

Waarom niet gewoon geheugen gebruiken? Wel, tijdelijke bestanden zijn perfect voor het omgaan met gegevens die te groot zijn voor RAM of wanneer je een fysiek bestand wil zonder je zorgen te maken over langdurige opslag of opruiming.

## Zie ook
- Clojure's eigen [IO documentatie](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Java's [Bestandsdocumentatie](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) — voor de basisdetails.
- Misschien een wandeling maken door [Java's NIO bestandspakket](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html) voor grootschalige en meer complexe bestandsoperaties, voorbij de basis.
