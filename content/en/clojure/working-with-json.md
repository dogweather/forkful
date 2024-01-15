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

## Why

JSON (JavaScript Object Notation) is a widely used data format for exchanging information between web services. Clojure, being a functional programming language, provides a powerful and concise way to work with JSON data. It allows developers to easily process and manipulate JSON objects, making it an essential skill for any Clojure programmer.

## How To

To work with JSON in Clojure, you first need to import the `clojure.data.json` library. This library provides functions for parsing and creating JSON data. Let's take a look at an example of parsing a JSON string into a Clojure data structure:

```Clojure
(require '[clojure.data.json :as json])

(def data "{\"name\": \"John\", \"age\": 25, \"interests\": [\"programming\", \"hiking\"]}")

(def parsed-data (json/read-str data))
```

In the above code, we use the `read-str` function from the `clojure.data.json` library to parse the JSON string into a Clojure map. We can then access the data using normal Clojure functions such as `get`:

```Clojure
(get parsed-data "name")   ;; Output: "John"
(get parsed-data "age")    ;; Output: 25
(get parsed-data "interests")  ;; Output: ["programming" "hiking"]
```

Similarly, we can create a JSON string from a Clojure data structure using the `write-str` function:

```Clojure
(json/write-str parsed-data)   ;; Output: "{\"name\":\"John\",\"age\":25,\"interests\":[\"programming\",\"hiking\"]}"
```

## Deep Dive

Clojure provides a powerful way to transform and process data using its core functions such as `map`, `filter`, and `reduce`. These functions can be used to easily manipulate JSON objects and extract specific data.

For example, let's say we want to extract the names of the people from a list of JSON objects. We can use the `map` function to achieve this:

```Clojure
(def people [{:name "John", :age 25} {:name "Jane", :age 30} {:name "Bob", :age 40}])

(map :name people)  ;; Output: ("John" "Jane" "Bob")
```

We can also use the `filter` function to find specific data from a JSON object based on a condition. For instance, if we want to find all the people above the age of 30, we can do so using the `filter` function:

```Clojure
(filter #(> (:age %) 30) people)   ;; Output: ({:name "Jane", :age 30} {:name "Bob", :age 40})
```

## See Also

- [clojure.data.json library documentation](https://github.com/clojure/data.json)
- [Clojure for Beginners: Working with JSON](https://www.braveclojure.com/clojure-for-the-brave-and-true/working-with-json/)