---
date: 2024-02-03 19:02:43.841401-07:00
description: 'How to: Clojure, being a JVM language, allows you to utilize Java String
  methods directly. Here''s a basic example of how to capitalize a string in Clojure.'
lastmod: '2024-03-13T22:44:59.730224-06:00'
model: gpt-4-0125-preview
summary: Clojure, being a JVM language, allows you to utilize Java String methods
  directly.
title: Capitalizing a string
weight: 2
---

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
