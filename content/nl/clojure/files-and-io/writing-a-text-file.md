---
title:                "Een tekstbestand schrijven"
aliases: - /nl/clojure/writing-a-text-file.md
date:                  2024-01-28T22:12:14.725118-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
