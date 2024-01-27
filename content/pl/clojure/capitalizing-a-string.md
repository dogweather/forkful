---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i Dlaczego?)

Capitalizing strings means changing the first letter of each word to uppercase. We do it to standardize data entries, for titles, or just to make text look cleaner and more formal.

## How to:
(Jak to zrobić:)

```Clojure
(defn capitalize-string [s]
  (clojure.string/capitalize s))

;; Example: Capitalizing a single word
(println (capitalize-string "clojure"))

;; Example: Capitalizing multiple words using split and map
(println (map capitalize-string (clojure.string/split "hello clojure world" #"\s")))

;; Output for single word:
;; "Clojure"

;; Output for multiple words:
;; ("Hello" "Clojure" "World")
```

## Deep Dive
(Zagłębienie się)

Historically, string capitalization isn't novel—it has roots in early typesetting practices. In Clojure, capitalization can be done using built-in functions like `clojure.string/capitalize`, or you can roll your own with `map` and `reduce` for more complex rules. 

The default `capitalize` function will only uppercase the first character of the entire string. For titling purposes, where you might want to capitalize each word, you might utilize `split`, `map`, and `capitalize` together, as shown in the example. Be mindful of limitations: this doesn't account for exceptions in titling like 'of', 'and', or 'the'. 

Implementation wise, when you capitalize a string in Clojure, it's usually done in a functional style—transforming the data without changing the original string (immutability). This is different from some languages that allow directly modifying the original string (mutability).

## See Also
(Zobacz także)

- [ClojureDocs on clojure.string/capitalize](https://clojuredocs.org/clojure.string/capitalize)
- [The Java Platform toUpperCase method (Clojure runs on the JVM)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())
- [Clojure Style Guide](https://guide.clojure.style/#capitalize)
