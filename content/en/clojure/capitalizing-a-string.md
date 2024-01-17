---
title:                "Capitalizing a string"
html_title:           "Clojure recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string simply means converting the first letter of each word in a sentence to uppercase. Programmers do this to make text more readable and aesthetically pleasing. It is also commonly used in data processing and formatting.

## How to:

```Clojure
;; using the Java library
(require '[clojure.string :as str])
(str/capitalize "this is a string") ;=> "This is a string"

;; using the capitalize function from the clojure.string namespace
(clojure.string/capitalize "another string") ;=> "Another string"

;; using string manipulation
(defn capitalize [s]
  (let [words (str/split s #" ")]
    (apply str (map #(str/capitalize %) words))))

(capitalize "yet another string") ;=> "Yet Another String"
```

## Deep Dive:

Capitalizing a string is a common practice in several programming languages. It has been used in English grammar for centuries to mark the beginning of a sentence or proper nouns. In programming, it is often used to improve readability of user inputs, data processing, and formatting of output strings.

An alternative to capitalizing a string is to use the ```capitalize-title``` function from the ```clojure.java.string``` namespace. This function follows the standard title case capitalization rules. However, for simple text manipulation, using the ```capitalize``` function from the ```clojure.string``` namespace is sufficient.

Clojure's ```capitalize``` function uses the ```String's``` ```toUpperCase``` method to convert the first character to uppercase. This method handles special characters and diacritics accurately in different languages.

## See Also:

- clojure.string API documentation: https://clojure.github.io/clojure/clojure.string-api.html
- Ways to capitalize strings in Clojure: https://purelyfunctional.tv/guide/clojure-handle-string-uppercase-lowercase/
- How to format strings in Clojure: https://clojuredocs.org/clojure.string/format