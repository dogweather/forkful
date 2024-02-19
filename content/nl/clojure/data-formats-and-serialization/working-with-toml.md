---
aliases:
- /nl/clojure/working-with-toml/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:00.486439-07:00
description: "Werken met TOML betekent dat je gegevens beheert in een minimaal \"\
  Tom's Obvious, Minimal Language\" formaat, populair voor configuratiebestanden vanwege\
  \ de\u2026"
lastmod: 2024-02-18 23:09:01.505330
model: gpt-4-0125-preview
summary: "Werken met TOML betekent dat je gegevens beheert in een minimaal \"Tom's\
  \ Obvious, Minimal Language\" formaat, populair voor configuratiebestanden vanwege\
  \ de\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met TOML betekent dat je gegevens beheert in een minimaal "Tom's Obvious, Minimal Language" formaat, populair voor configuratiebestanden vanwege de gemakkelijke leesbaarheid. Programmeurs gebruiken het voor eenvoudig configuratiebeheer dat direct uit de verpakking werkt met een gebruiksvriendelijke syntaxis.

## Hoe:
Om met TOML in Clojure te werken, heb je een bibliotheek zoals `clj-toml` nodig. Voeg het eerst toe aan je `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Analyseer vervolgens wat TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Voorbeeld van TOML'")

(def parsed-config (toml/parse-string config-str))

;; Haal de titel uit de geanalyseerde TOML
(println (:title parsed-config)) ;; Uitvoer: Voorbeeld van TOML
```

Om TOML te genereren:

```clojure
(def data {:title "Voorbeeld van TOML"})

(println (toml/generate-string data))
;; Uitvoer: title = "Voorbeeld van TOML"
```

## Diepgaande Duik
TOML werd rond 2013 gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, als een eenvoudiger alternatief voor YAML en JSON voor configuratiebestanden. Het streeft naar helderheid en is bedoeld als een specificatie die mensen zonder extra hulpmiddelen kunnen lezen.

Hoewel JSON vaak wordt gebruikt voor API's en webapplicaties, en YAML complex kan worden met referenties en scriptmogelijkheden, onderscheidt TOML zich met een focus op eenvoudige, tabelgebaseerde structuren. Deze eenvoud maakt het vooral populair in de Rust-gemeenschap en andere moderne programmeeromgevingen.

Clojure, met zijn focus op eenvoud en praktische bruikbaarheid, past goed bij TOML voor configuratie. `clj-toml` of alternatieve bibliotheken overbruggen de kloof. Ze vertalen de statische gegevens van TOML naar de dynamische, functionele wereld van Clojure.

## Zie Ook
- TOML's GitHub Repo: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` op Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure Documentatie: [clojure.org](https://clojure.org/guides/getting_started)
- Introductie tot `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
