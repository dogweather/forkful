---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:30.983609-07:00
description: "YAML, \"YAML Ain't Markup Language,\" is een mensvriendelijke gegevensserialisatiestandaard\
  \ voor alle programmeertalen. Programmeurs gebruiken YAML voor\u2026"
lastmod: 2024-02-19 22:05:09.529534
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language,\" is een mensvriendelijke gegevensserialisatiestandaard\
  \ voor alle programmeertalen. Programmeurs gebruiken YAML voor\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, "YAML Ain't Markup Language," is een mensvriendelijke gegevensserialisatiestandaard voor alle programmeertalen. Programmeurs gebruiken YAML voor configuratiebestanden en gegevensuitwisseling waar leesbaarheid belangrijk is.

## Hoe te:

Clojure bevat geen ingebouwde ondersteuning voor YAML. Je zult een bibliotheek zoals `clj-yaml` moeten gebruiken. Voeg het eerst toe aan je afhankelijkheden:

```clojure
;; Toevoegen aan project.clj of deps.edn
[clj-yaml "0.7.0"]
```

Nu, laten we een YAML string naar een Clojure map parseren en vice versa:

```clojure
(require '[clj-yaml.core :as yaml])

;; YAML string naar Clojure map parseren
(let [yaml-str "foo: bar\nbaz: 42"]
  (yaml/parse-string yaml-str))
;; => {"foo" "bar", "baz" 42}

;; Clojure map naar YAML converteren
(let [clojure-map {"foo" "bar", "baz" 42}]
  (yaml/generate-string clojure-map))
;; Geeft YAML string uit:
;; foo: bar
;; baz: 42
```

## Diepere Duik

YAML werd voor het eerst uitgebracht in 2001, met als doel menselijker leesbaar te zijn dan XML en tegelijkertijd rijkere gegevensstructuren te bieden dan JSON. `clj-yaml` is gebouwd op SnakeYAML, een Java-bibliotheek, waardoor het interoperabel is met JVM-talen. Alternatieven zijn het direct gebruik van `org.yaml.snakeyaml` of `cheshire` voor JSON-conversie, aangezien JSON een subset is van YAML.

## Zie Ook

Duik dieper met deze bronnen:

- Officiële site van YAML: [https://yaml.org](https://yaml.org)
- Github voor clj-yaml: [https://github.com/clj-commons/clj-yaml](https://github.com/clj-commons/clj-yaml)
- SnakeYAML Engine: [https://bitbucket.org/asomov/snakeyaml-engine](https://bitbucket.org/asomov/snakeyaml-engine)
