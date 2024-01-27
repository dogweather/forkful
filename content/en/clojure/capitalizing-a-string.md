---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means making the first letter uppercase and the rest lowercase. We do it to normalize data and improve readability, like turning 'alice' into 'Alice' for names.

## How to:
In Clojure, there's no built-in function to directly capitalize strings. You roll your own with `clojure.string` library. Here's a quick way:

```clojure
(require '[clojure.string :as str])

(defn capitalize [s]
  (when s
    (str/capitalize s)))

(capitalize "hello world") ; => "Hello world"
```

Sample output for `capitalize` function:

```clojure
(capitalize "clojure") ; => "Clojure"
(capitalize "123clojure") ; => "123clojure"
(capitalize "") ; => nil
(capitalize nil) ; => nil
```

## Deep Dive
Clojure's standard library, `clojure.string`, prefers simplicity. Thus, no ready-made `capitalize` function like you'd find in other languages. Historically, Clojure leans on Java's String methods, which offer basic manipulation, but not `capitalize`. 

This lack pushes you towards either writing your own solution, like above, or leveraging external libraries. There's also `capitalize` from `clojure.contrib.string`, a historical separate contrib library before being deprecated and partly merged with clojure.string in later versions.

The `str/capitalize` function's simplicity means it only concerns itself with the first character. For more nuanced capitalization, like title-case or handling international characters, you must write a custom solution or grab a Java library.

Here's an alternate custom function that handles strings with multiple words:

```clojure
(defn title-case [s]
  (->> s
       (str/split #"\s+")
       (map str/capitalize)
       (str/join " ")))

(title-case "the lord of the rings") ; => "The Lord Of The Rings"
```

Again, internationalization (i18n) isn't covered here; handling Unicode correctly is a whole other beast, often requiring specialized libraries.

## See Also
- Clojure Strings API: https://clojure.github.io/clojure/clojure.string-api.html
- Java String Documentation: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Clojure Contrib library (archived): https://github.com/clojure/clojure-contrib
- `clojure.string` source code: https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj
