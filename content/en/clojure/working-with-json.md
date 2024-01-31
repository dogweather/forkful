---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a data format used to store and transport data. Programmers use JSON because of its ease of use with web APIs and its language-independent text format which makes it super handy for data interchange.

## How to:
Let's play with JSON in Clojure. You'll need `Cheshire`, a popular library for JSON encoding/decoding.

First, add Cheshire to your `project.clj` dependencies:
```clojure
[cheshire "5.10.1"]
```

Reading JSON from a string and converting it into a Clojure map:
```clojure
(require '[cheshire.core :as json])

(def json-str "{\"name\":\"Clojure\"}")
(def clojure-map (json/parse-string json-str))

(println clojure-map)  ;; => {"name" "Clojure"}
```

Turning a Clojure map into a JSON string:
```clojure
(def clojure-data {:language "Clojure" :cool true})
(def json-output (json/generate-string clojure-data))

(println json-output)  ;; => {"language":"Clojure","cool":true}
```

Parsing JSON from a file:
```clojure
(slurp "data.json")  ;; contents: {"message": "Hello, JSON!"}
(def file-content (slurp "data.json"))
(def message-data (json/parse-string file-content true))

(println message-data)  ;; => {"message" "Hello, JSON!"}
```

## Deep Dive
JSON's history starts with JavaScript, but now it's everywhere, not dependent on its parent language. Alternatives? XML was the go-to before, more verbose though. YAML's simpler, human-friendlier but not as universal for APIs. Implementation-wise: Clojure's not JavaScript, so libraries like Cheshire are essential. They bridge the gap by using Java libraries underneath to handle the parsing and generation efficiently.

## See Also
- [Cheshire GitHub Repo](https://github.com/dakrone/cheshire): For library details and updates.
- [JSON.org](https://www.json.org): JSON specs and details.
- [Clojure from the ground up: JSON](https://aphyr.com/posts/305-clojure-from-the-ground-up-json): A detailed guide on handling JSON with Clojure.
