---
title:                "Working with JSON"
date:                  2024-02-03T19:03:12.908663-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON (JavaScript Object Notation) in Clojure involves parsing JSON strings into Clojure data structures (maps, vectors) and vice versa. This task is fundamental for web services, APIs, and applications that need to communicate data in a structured, text-based format because JSON is universally recognized and supported across different programming environments.

## How to:
Clojure does not include built-in functions for working with JSON, so you will typically use third-party libraries. `cheshire` and `jsonista` are popular choices due to their ease of use and performance.

### Using Cheshire
First, add Cheshire to your project dependencies in `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

To parse a JSON string into a Clojure map and convert a map to a JSON string:

```clj
(require '[cheshire.core :as json])

;; Parse JSON string to Clojure map
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Convert Clojure map to JSON string
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Using Jsonista
Add Jsonista to your project `project.clj`:
```clj
[jsonista "0.3.2"]
```

Similar operations with Jsonista:

```clj
(require '[jsonista.core :as j])

;; Parse JSON string to Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Convert Clojure map to JSON string
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

In both libraries, you have the option to encode and decode more complex data structures, and there are additional functions and parameters that allow for customization of the serialization and deserialization processes. For most applications, the demonstrated functionality provides a solid foundation for working with JSON in Clojure applications.
