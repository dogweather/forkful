---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON er et dataformat vi bruker for å lagre og utveksle data. Programmerere trenger det for å snakke med webtjenester og lagre konfigurasjoner.

## How to:
I Clojure bruker vi ofte `cheshire` biblioteket for å håndtere JSON. La oss se hvordan man kan parse og generere JSON.
```Clojure
(require '[cheshire.core :as json])

;; Parsing JSON streng til Clojure map
(defn parse-json [json-str]
  (json/parse-string json-str true))

(parse-json "{\"name\": \"Ola\", \"age\": 30}")
;; => {"name" "Ola", "age" 30}

;; Generer en JSON streng fra en Clojure map
(defn generate-json [clojure-map]
  (json/generate-string clojure-map))

(generate-json {"name" "Kari" "age" 28})
;; => "{\"name\":\"Kari\",\"age\":28}"
```

## Deep Dive
JSON står for JavaScript Object Notation og ble opprinnelig brukt i JavaScript. Nå er det språkuavhengig og en webstandard. Alternativer til JSON inkluderer XML og YAML, men JSON er ofte foretrukket for sin letthet og hastighet. Når du jobber med JSON i Clojure, må man huske på konverteringen mellom JSON-typene og Clojure-typene, som strenger, booleans, tall, lister (arrays) og maps (objekter).

## See Also
- Cheshire GitHub: https://github.com/dakrone/cheshire
- Clojure JSON-doc: https://clojuredocs.org/clojure.data.json
- JSON officiell webbsida: https://www.json.org/json-no.html