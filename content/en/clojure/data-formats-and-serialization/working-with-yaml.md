---
date: 2024-02-03 19:03:13.563232-07:00
description: "How to: Clojure does not include built-in support for YAML, but you\
  \ can utilize third-party libraries such as `clj-yaml` for parsing and generating\
  \ YAML\u2026"
lastmod: '2024-03-13T22:44:59.763632-06:00'
model: gpt-4-0125-preview
summary: Clojure does not include built-in support for YAML, but you can utilize third-party
  libraries such as `clj-yaml` for parsing and generating YAML data.
title: Working with YAML
weight: 41
---

## How to:
Clojure does not include built-in support for YAML, but you can utilize third-party libraries such as `clj-yaml` for parsing and generating YAML data. First, add the library to your project dependencies:

```clojure
;; Add this to your project.clj dependencies
[clj-yaml "0.7.0"]
```

Here's how you can use `clj-yaml` to parse YAML and convert Clojure maps to YAML.

### Parsing YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; Parsing a YAML string
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Output:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generating YAML from Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; Converting a Clojure map to a YAML string
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Output:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

These simple operations with `clj-yaml` can be integrated into Clojure applications to handle configuration files or facilitate data exchange with other services or components that use YAML.
