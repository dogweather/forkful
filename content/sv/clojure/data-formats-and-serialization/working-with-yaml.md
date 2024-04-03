---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:57.403889-07:00
description: "Hur man: Clojure inkluderar inte inbyggt st\xF6d f\xF6r YAML, men du\
  \ kan anv\xE4nda tredjepartsbibliotek som `clj-yaml` f\xF6r att tolka och generera\
  \ YAML-data.\u2026"
lastmod: '2024-03-13T22:44:37.545459-06:00'
model: gpt-4-0125-preview
summary: "Clojure inkluderar inte inbyggt st\xF6d f\xF6r YAML, men du kan anv\xE4\
  nda tredjepartsbibliotek som `clj-yaml` f\xF6r att tolka och generera YAML-data."
title: Att Arbeta med YAML
weight: 41
---

## Hur man:
Clojure inkluderar inte inbyggt stöd för YAML, men du kan använda tredjepartsbibliotek som `clj-yaml` för att tolka och generera YAML-data. Först, lägg till biblioteket i dina projektberoenden:

```clojure
;; Lägg detta till dina beroenden i project.clj
[clj-yaml "0.7.0"]
```

Så här kan du använda `clj-yaml` för att tolka YAML och konvertera Clojure-mapar till YAML.

### Tolka YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; Tolkar en YAML-sträng
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Utdata:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generera YAML från Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; Konverterar en Clojure-map till en YAML-sträng
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Utdata:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Dessa enkla operationer med `clj-yaml` kan integreras i Clojure-applikationer för att hantera konfigurationsfiler eller underlätta datautbyte med andra tjänster eller komponenter som använder YAML.
