---
title:                "Working with yaml"
html_title:           "Clojure recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

YAML is a popular data serialization format that is used in various programming languages, including Clojure. It offers a human-readable and easy-to-learn syntax, making it a preferred choice for configuring applications and sharing data between systems.

## How To

To work with YAML in Clojure, we first need to add the "org.clojure/data.xml" dependency to our project. Then, we can use the `clojure.data.xml` namespace and its functions to read and parse YAML files.

```Clojure
(ns my-project.core
  (:require [clojure.data.xml :as xml]))

(defn read-yaml [file]
  (let [content (xml/parse file)]
    content))

(defn print-yaml [yaml-data]
  (xml/emit yaml-data))

;; reading a YAML file
(def my-yaml (read-yaml "my-yaml-file.yml"))

;; printing the YAML data in a readable format
(print-yaml my-yaml)
```

Output:

```Clojure
{:key1 "value1",
 :key2 "value2",
 :key3 ["value3" "value4"],
 :key4 {:nested-key1 "nested-value1",
        :nested-key2 "nested-value2"}}
```

## Deep Dive

YAML supports a wide range of data types, including strings, numbers, lists, maps, and even custom types. In Clojure, YAML data is parsed into Clojure data structures, such as maps and vectors, making it easy to work with and manipulate.

Additionally, YAML also allows for the inclusion of multiple documents in a single file, which can be useful when dealing with complex data structures. Clojure's `clojure.data.xml` includes functions to read and parse multiple YAML documents from a single file.

Parsing and printing YAML in Clojure is straightforward, but we can also convert YAML data into other formats using `clojure.data.xml`. For example, we can convert YAML into JSON or EDN (Clojure's native data format) using the `xml/emit-json` and `xml/emit-edn` functions respectively.

## See Also

- [Clojure YAML Documentation](https://clojure.github.io/data.xml/), for more information about `clojure.data.xml` and its functions.
- [YAML Official Website](https://yaml.org/), for a complete specification and documentation of the YAML format.
- [Clojure Official Website](https://clojure.org/), for more information about the Clojure programming language.