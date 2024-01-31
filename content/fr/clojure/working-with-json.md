---
title:                "Manipulation de JSON"
date:                  2024-01-19
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON c'est le format d'échange de données. Les devs l'utilisent pour sa simplicité et interopérabilité entre systèmes.

## How to:
**Lire un JSON:**
```Clojure
(require '[clojure.data.json :as json])

(defn lire-json [json-str]
  (json/read-str json-str :key-fn keyword))

(lire-json "{\"name\":\"Dupont\",\"age\":42}")
;; => {:name "Dupont", :age 42}
```

**Écrire en JSON:**
```Clojure
(require '[clojure.data.json :as json])

(defn ecrire-json [data]
  (json/write-str data))

(ecrire-json {:name "Dupont" :age 42})
;; => "{\"name\":\"Dupont\",\"age\":42}"
```

## Deep Dive
JSON ou JavaScript Object Notation a été introduit en 2001. YAML et XML sont des alternatives, mais JSON domine pour sa facilité de lecture et de mapping à des structures de données. En Clojure, `clojure.data.json` permet de sérialiser et désérialiser efficacement grâce à son intégration avec la plateforme JVM.

## See Also
- ClojureDocs pour `clojure.data.json`: https://clojuredocs.org/clojure.data.json
- Tutoriel JSON en Clojure: https://www.learn-clojure.com/working_with_json/
- Spécification JSON: https://www.json.org/json-fr.html
