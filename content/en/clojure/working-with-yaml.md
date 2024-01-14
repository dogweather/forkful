---
title:                "Clojure recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

When it comes to managing data, YAML has become a popular choice for many developers due to its simplicity and human-readable format. Whether you are working with configuration files or storing data, YAML provides a flexible and intuitive solution.

## How To

To start working with YAML in your Clojure projects, you will first need to add the [clj-yaml](https://github.com/lancepantz/clj-yaml) library as a dependency in your project. This library provides functions for reading, writing, and parsing YAML data.

Once the library is added, you can start working with YAML by using the `load` function to read a YAML file and convert it into a Clojure data structure:

```Clojure
(require '[clj-yaml.core :as yaml])

(def data (yaml/load "config.yml"))
```

This will return a map containing the YAML data, which you can then manipulate as needed. To write data to a YAML file, you can use the `dump` function:

```Clojure
(yaml/dump data "output.yml")
```

This will write the data to a new YAML file called "output.yml". You can also convert Clojure data structures to YAML strings using the `generate-string` function.

## Deep Dive

One of the benefits of working with YAML in Clojure is the ability to use custom tags to define how data should be parsed. This can be helpful when working with complex data or integrating with other systems that use YAML.

For example, you can define a custom tag for parsing dates in a specific format:

```Clojure
(require '[clj-yaml.core :as yaml])

(def custom-tags {"!date" (fn [data] (parse-date data))})

(def data (yaml/load "config.yml" :custom-tags custom-tags))
```

This will call the `parse-date` function whenever a `!date` tag is encountered in the YAML file.

Another useful feature is the ability to merge multiple YAML files into a single data structure using the `merge` function:

```Clojure
(require '[clj-yaml.core :as yaml])

(def data (yaml/merge "defaults.yml" "config.yml"))
```

This will merge the data from both files, with values in the second file taking precedence over the first.

## See Also

For more information on working with YAML in Clojure, you can refer to the [official documentation](https://github.com/lancepantz/clj-yaml) and also check out these helpful resources:

- [Working with YAML in Clojure](https://lithic.tech/blog/2020/working-with-yaml-in-clojure)
- [YAML Syntax and Usage](https://yaml.org/spec/1.2/spec.html#id2760844)
- [Using YAML for Configuration in Clojure Projects](https://purelyfunctional.tv/guide/yaml/)