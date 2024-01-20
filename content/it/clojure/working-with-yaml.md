---
title:                "Lavorare con YAML"
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language", è un formato di serializzazione leggibile. I programmatori lo usano per configurazione, data dumping e come punto di contatto tra diversi linguaggi.

## How to:
Per lavorare con YAML in Clojure, servono `clj-yaml` e `clojure.java.io`. Installa `clj-yaml` con Leiningen o deps.edn.

```Clojure
;; deps.edn
{:deps {clj-yaml {:mvn/version "0.7.0"}}}
```

Leggi YAML:

```Clojure
(require '[clj-yaml.core :as yaml])
(require '[clojure.java.io :as io])

;; Legge il file YAML e lo trasforma in una mappa Clojure.
(def config-map (yaml/parse-string (slurp (io/resource "config.yaml"))))
```

Scrivi YAML:

```Clojure
(defn save-yaml [data filepath]
  (spit filepath (yaml/generate-string data)))

;; Uso
(save-yaml config-map "output.yaml")
```

## Deep Dive
YAML nasce nel 2001 per essere leggibile e utile per tutte le programmazioni. Alternativa: JSON, meno leggibile ma più veloce. Per performance, Clojure usa la libreria SnakeYAML, scritta in Java, per parsing e rendering YAML.

## See Also
- Documentazione `clj-yaml`: https://github.com/clj-commons/clj-yaml
- Guida YAML ufficiale: https://yaml.org/
- Comparazione YAML e JSON: https://phoenixnap.com/kb/yaml-vs-json