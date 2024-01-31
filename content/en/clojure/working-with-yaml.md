---
title:                "Working with YAML"
date:                  2024-01-19
simple_title:         "Working with YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, "YAML Ain't Markup Language," is a human-friendly data serialization standard for all programming languages. Programmers use YAML for config files and data exchange where readability matters.

## How to:

Clojure doesn't include built-in support for YAML. You’ll need to use a library like `clj-yaml`. First, add it to your dependencies:

```clojure
;; Add to project.clj or deps.edn
[clj-yaml "0.7.0"]
```

Now, let's parse a YAML string into a Clojure map and vice versa:

```clojure
(require '[clj-yaml.core :as yaml])

;; Parsing YAML string to Clojure map
(let [yaml-str "foo: bar\nbaz: 42"]
  (yaml/parse-string yaml-str))
;; => {"foo" "bar", "baz" 42}

;; Converting Clojure map to YAML
(let [clojure-map {"foo" "bar", "baz" 42}]
  (yaml/generate-string clojure-map))
;; Outputs YAML string:
;; foo: bar
;; baz: 42
```

## Deep Dive

YAML was first released in 2001, aiming to be more human-readable than XML while providing richer data structures than JSON. `clj-yaml` is built atop SnakeYAML, a Java library, allowing interoperability with JVM languages. Alternatives include directly using `org.yaml.snakeyaml` or `cheshire` for JSON conversion, since JSON is a subset of YAML.

## See Also

Dive deeper with these resources:

- YAML official site: [https://yaml.org](https://yaml.org)
- Github for clj-yaml: [https://github.com/clj-commons/clj-yaml](https://github.com/clj-commons/clj-yaml)
- SnakeYAML Engine: [https://bitbucket.org/asomov/snakeyaml-engine](https://bitbucket.org/asomov/snakeyaml-engine)
