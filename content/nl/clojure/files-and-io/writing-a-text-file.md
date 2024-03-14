---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:14.725118-07:00
description: "Het schrijven van een tekstbestand omvat het cre\xEBren of wijzigen\
  \ van tekstgegevens en deze opslaan in een bestand op uw opslagmedium. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-13T22:44:50.438447-06:00'
model: gpt-4-0125-preview
summary: "Het schrijven van een tekstbestand omvat het cre\xEBren of wijzigen van\
  \ tekstgegevens en deze opslaan in een bestand op uw opslagmedium. Programmeurs\
  \ doen\u2026"
title: Een tekstbestand schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Het schrijven van een tekstbestand omvat het creëren of wijzigen van tekstgegevens en deze opslaan in een bestand op uw opslagmedium. Programmeurs doen dit voor datalogging, configuratie-instellingen of het exporteren van menselijk leesbare rapporten.

## Hoe te:

In Clojure gebruik je de `spit` functie om gegevens naar een tekstbestand te schrijven. Het is eenvoudig:

```clojure
(spit "voorbeeld.txt" "Hallo, Wereld! Dit is Clojure aan het woord.")
```

De `spit` functie neemt de bestandsnaam en de inhoud. Om inhoud toe te voegen, stel je de `append` vlag in:

```clojure
(spit "voorbeeld.txt" "\nLaten we deze nieuwe regel toevoegen." :append true)
```

Voorbeelduitvoer voor `voorbeeld.txt` na beide bewerkingen:

```
Hallo, Wereld! Dit is Clojure aan het woord.
Laten we deze nieuwe regel toevoegen.
```

## Diepere Duik

Clojure's `spit` functie komt uit zijn "I/O" bibliotheek - een opvolger van Lisp's nalatenschap van bondige bestandsbewerkingen. Alternatieven in Clojure omvatten `clojure.java.io/writer` voor gebufferd schrijven en bibliotheken zoals `slurp` voor het lezen van bestanden. Wanneer je `spit` gebruikt, onthoud dan dat het niet bedoeld is voor grote stromen gegevens vanwege potentiële geheugenproblemen - gebruik in plaats daarvan `writer` en loop over de gegevens.

## Zie Ook

- Clojure Docs voor `spit`: [https://clojuredocs.org/clojure.core/spit](https://clojuredocs.org/clojure.core/spit)
- Clojure `java.io` wrapper: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
