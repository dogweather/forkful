---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means changing all the uppercase characters in a string to their lowercase counterparts. Programmers often do this to make string comparisons case-insensitive and for data normalization.

## How to:

Clojure provides an in-built function `clojure.string/lower-case` to convert a string to lowercase.

```clojure
(require '[clojure.string :as str])

(def s "HeLLo WoRLD")

(str/lower-case s)
```

When you run the above code, it transforms the string "HeLLo WoRLD" into "hello world".

## Deep Dive:

Clojure's `lower-case` function is actually a wrapper around Java's `toLowerCase` method on strings. So not only does it benefit from the simplicity of being a Clojure function, it also enjoys the thoroughness with which Java handles case conversion, taking into account locale-specific rules. Of course, this means your output may vary based on the JVM's default locale setting.

If, for some reason, you can't (or don't want to) use the `lower-case` function, you can reimplement it manually through a combination of `map` with the `Character/lowerCase` function and a join.

```clojure
(defn lower-case [s]
  (->> s
       (map #(Character/toLowerCase %))
       (apply str)))

(lower-case "HeLLo WoRLD")
```

This version will likely be slower (since it doesn't benefit from the JVM's optimized string handling) but it could be useful if you're in a context where you can't use clojure.string.

Although both `clojure.string/lower-case` and `Character/lowerCase` are great, be cautious about underestimating Unicode's habit of piling on surprises. 

## See Also:

- To read more about the `lower-case` function and its alternatives, head over to [clojure.string API documentation](https://clojuredocs.org/clojure.string/lower-case).
- For a deep dive into Clojure's string handling and Unicode complexities, [this article](https://clojure.org/guides/weird_characters) is a great place to dig in.
- To see `toLowerCase` method and its usage in Java, refer to [Java String toLowerCase() Method](https://www.javatpoint.com/java-string-tolowercase).