---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON è un formato di dati usato per scambiare informazioni facilmente tra sistemi. I programmatori lo usano per la sua leggibilità e la facilità di mappare direttamente su strutture di dati.

## How to:
Clojure tratta i JSON con librerie esterne come Cheshire. Per usarla, aggiungi `[cheshire "5.10.1"]` alle tue dipendenze.

```Clojure
(require '[cheshire.core :as json])

;; Converti un oggetto Clojure in una stringa JSON
(def my-map {:name "Luca" :age 30})
(def my-json (json/generate-string my-map))
;; my-json => "{\"name\":\"Luca\",\"age\":30}"

;; Parse di una stringa JSON in una mappa Clojure
(def my-parsed-map (json/parse-string my-json true))
;; my-parsed-map => {:name "Luca", :age 30}
```

## Deep Dive
JSON (JavaScript Object Notation) fu creato agli inizi del 2000. Alternativa a XML, è più semplice e leggero. In Clojure, si tende a preferire JSON per le API Web, grazie alla sua compatibilità con JavaScript. Cheshire sfrutta Jackson, una libreria Java performante per la serializzazione e deserializzazione JSON.

## See Also
- Cheshire su GitHub: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- Jackson Project su GitHub: [https://github.com/FasterXML/jackson](https://github.com/FasterXML/jackson)
