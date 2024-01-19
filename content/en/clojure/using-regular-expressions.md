---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular Expressions (RegEx) is a general method to match patterns in text. Programmers use it to detect, extract, and replace string parts based on these patterns.

## How to:

Clojure includes Java's `java.util.regex` library for RegEx usage. 

Match regex in a string using `re-matches`:

```clojure
(re-matches #"\d+" "1234") ; "1234"
(re-matches #"\d+" "abc") ; nil
```

Find regex with `re-find`:

```clojure
(re-find #"\d+" "abc123def") ; "123"
```

Replace regex using `re-seq`:

```clojure
(clojure.string/replace "abc123def" #"\d+" "000") ; "abc000def"
```

## Deep Dive 

Clojure’s RegEx utility traces its root back to Java’s `java.util.regex` package, which was borne out of Perl's rich RegEx expressions. The key difference lies in Clojure's immutable and higher-order functions.

Clojure allows RegEx usage without needing external libraries, while in Java you'll need the latter for complex patterns.

`re-pattern` compiles a RegEx string, good for dynamic patterns:

```clojure
(re-matches (re-pattern (str "\\d" "+")) "123") ; "123"
```

## See Also 

For more robust RegEx utility in Clojure, check out the [re-pattern](https://clojuredocs.org/clojure.core/re-pattern) function at Clojure Docs.

Familiarize yourself with [Clojure's String API](https://clojuredocs.org/quickref/Clojure%20Core) for more string manipulating goodness.

Finally, master your RegEx skills at [RegexOne](https://regexone.com/). They offer interactive lessons with plenty of examples in different languages, including Clojure.