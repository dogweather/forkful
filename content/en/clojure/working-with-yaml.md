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

## What & Why?
Working with YAML (YAML Ain't Markup Language) is a common practice for programmers, as it allows for easy and human-readable data serialization. YAML is well-suited for writing configuration files, as well as for storing and transferring structured data. It is also a popular choice for creating data-driven applications.

## How to:
To work with YAML in Clojure, you will need to use a library called clj-yaml. First, you will need to add it as a dependency in your project.clj file:
```Clojure
[clj-yaml "0.5.1"]
```
Next, require the library in your namespace:
```Clojure
(ns my-project.core
  (:require [clj-yaml.core :as yaml]))
```
Now, you can use the functions provided by the library to read and write YAML data. Here's an example of reading a YAML file and converting it into a Clojure map:
```Clojure
(defn read-yaml [filepath]
  (yaml/parse-string (slurp filepath)))
  
(def data (read-yaml "config.yml"))
```
And here's an example of writing a YAML file from a Clojure map:
```Clojure
(defn write-yaml [data filepath]
  (with-open [f (io/writer filepath)]
    (yaml/emit data f)))
    
(write-yaml {"name" "John" "age" 28} "person.yml")
```

## Deep Dive:
YAML was originally designed by Clark Evans in 2001, with the goal of creating a human-friendly data serialization format that is easy to read and modify. It is based on the syntax of Python, and borrows concepts from other programming languages such as Perl, Lisp, and C. YAML is a text file format, with the extension ".yaml" or ".yml", and uses indentation to define its data structure.

There are other popular data serialization formats, such as JSON and XML, but YAML differs in its emphasis on readability and its ability to handle complex and nested data structures. However, it should be noted that YAML is not suitable for all use cases, and it may not be the best choice for large amounts of data or situations where performance is critical.

The clj-yaml library is built on top of the popular Java library SnakeYAML, which provides fast and efficient parsing of YAML data. This ensures that working with YAML in Clojure is both easy and performant.

## See Also:
- Official YAML website: https://yaml.org
- clj-yaml library documentation: https://clojars.org/clj-yaml
- SnakeYAML library documentation: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation