---
title:                "Working with json"
html_title:           "Clojure recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON in Clojure is the process of manipulating data in the JSON format using the Clojure programming language. JSON stands for JavaScript Object Notation and it is commonly used for transmitting data between a server and a web application. Programmers use JSON in Clojure to easily parse and extract data from API responses or to create JSON data to be sent to a server.

## How to:

To work with JSON in Clojure, we first need to require the `clojure.data.json` library. Then, we can use the `parse` function to convert a JSON string into a Clojure data structure:

```Clojure
(require '[clojure.data.json :as json])

(def json-string "{\"name\": \"John\", \"age\": 30}")

(json/parse-string json-string)
;; => {:name "John", :age 30}
```

We can also use the `slurp` function to read a JSON file and parse it directly into a Clojure data structure:

```Clojure
(def json-file (slurp "data.json"))

(json/parse-string json-file)
;; => {:name "John", :age 30}
```

To convert a Clojure data structure into a JSON string, we can use the `generate-string` function:

```Clojure
(require '[clojure.data.json :as json])

(def person {:name "John", :age 30})

(json/generate-string person)
;; => "{\"name\":\"John\",\"age\":30}"
```

## Deep Dive

JSON was first introduced in 2001 as a lightweight alternative to XML for transmitting data over the internet. It is a popular format for web applications due to its human-readable syntax and widespread support among programming languages.

In addition to the `clojure.data.json` library, there are other libraries available for working with JSON in Clojure, such as `cheshire` and `jsonista`. These libraries may offer additional features or performance optimizations.

When working with large JSON files, it is recommended to use streaming parsers instead of loading the entire file into memory. The `cheshire` library provides this capability with the `parse-stream` function.

## See Also

- [Clojure data.json documentation](https://github.com/clojure/data.json)
- [Cheshire library for JSON in Clojure](https://github.com/dakrone/cheshire)
- [Jsonista library for JSON in Clojure](https://github.com/metosin/jsonista)