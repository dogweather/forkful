---
title:                "Clojure recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON, or JavaScript Object Notation, is a popular file format used for storing and transmitting data. It is a lightweight, human-readable format that is easy for both humans and machines to understand. As a result, it has become the preferred format for data exchange in web applications and APIs.

## How To
When working with JSON in Clojure, there are a few key things to keep in mind. First, we need to understand that JSON is just a text-based format, so we will need to use functions to convert it to and from Clojure data structures. Secondly, Clojure has a built-in library called `clojure.data.json` that makes working with JSON a breeze.

Let's take a look at a basic example of how to convert a Clojure map into JSON and vice versa:

```Clojure
(require '[clojure.data.json :as json])

(def user {:name "John", :age 30, :occupation "Software Engineer"})

(json/write-str user)
;; Output: "{\"name\":\"John\",\"age\":30,\"occupation\":\"Software Engineer\"}"

(json/read-str "{\"name\":\"Jane\",\"age\":28,\"occupation\":\"Data Scientist\"}")
;; Output: {:name "Jane", :age 28, :occupation "Data Scientist"}
```

As you can see, the `clojure.data.json` library provides us with `write-str` and `read-str` functions to convert between Clojure data structures and JSON strings. We can also use the `write` and `read` functions to convert to and from files.

## Deep Dive
Clojure's `clojure.data.json` library comes with several functions that allow us to manipulate JSON data. Some of the most commonly used ones include `parse`, `format`, `compact`, `encode`, and `decode`. These functions provide us with a way to parse JSON strings, pretty-print JSON data, remove whitespace, and encode and decode JSON data.

Another useful function is `json-str`, which allows us to convert any Clojure data structure into a JSON string. This can be especially handy when building APIs that need to return JSON data.

It is also worth noting that Clojure's `cheshire` library provides an alternative to `clojure.data.json` with additional features such as support for custom encoders and decoders, better error handling, and improved performance.

## See Also
- Official Clojure documentation for `clojure.data.json`: https://clojure.github.io/data.json/
- Cheshire library for working with JSON in Clojure: https://github.com/dakrone/cheshire