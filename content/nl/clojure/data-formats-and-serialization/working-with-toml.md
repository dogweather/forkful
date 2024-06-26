---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:00.486439-07:00
description: 'Hoe: Om met TOML in Clojure te werken, heb je een bibliotheek zoals
  `clj-toml` nodig. Voeg het eerst toe aan je `deps.edn`.'
lastmod: '2024-03-13T22:44:50.443300-06:00'
model: gpt-4-0125-preview
summary: Om met TOML in Clojure te werken, heb je een bibliotheek zoals `clj-toml`
  nodig.
title: Werken met TOML
weight: 39
---

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
