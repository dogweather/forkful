---
title:                "Capitalizing a string"
aliases:
- en/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:02:43.841401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string involves modifying the string so that its first character is uppercase, while the rest of the string remains unchanged. Programmers often perform string capitalization to ensure data consistency, especially for names and places or to comply with grammatical rules in user interfaces.

## How to:
Clojure, being a JVM language, allows you to utilize Java String methods directly. Here's a basic example of how to capitalize a string in Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure does not include a built-in function specifically for capitalizing strings, but as shown, you can easily achieve this by combining `clojure.string/upper-case`, `subs`, and `str` functions.

For a more concise solution and handling more complex string manipulations, you might turn to a third-party library. One such popular library in the Clojure ecosystem is `clojure.string`. However, as of my last update, it doesn't offer a direct `capitalize` function beyond what's demonstrated with core Clojure functionalities, so the method shown above is your straightforward approach without pulling in additional libraries specifically for capitalization.

Remember, when working with strings in Clojure that interact with Java methods, you're effectively working with Java strings, enabling you to leverage the entire arsenal of Java's String methods directly in your Clojure code if necessary.
