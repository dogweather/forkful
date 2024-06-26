---
date: 2024-01-20 17:37:54.271048-07:00
description: 'How to: In Clojure, to convert a string to lower case, you''ll use the
  `clojure.string/lower-case` function. Look how simple it is.'
lastmod: '2024-03-13T22:44:59.733802-06:00'
model: gpt-4-1106-preview
summary: In Clojure, to convert a string to lower case, you'll use the `clojure.string/lower-case`
  function.
title: Converting a string to lower case
weight: 4
---

## How to:
In Clojure, to convert a string to lower case, you'll use the `clojure.string/lower-case` function. Look how simple it is:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

Output is straightforward:

```clojure
"hello, world!"
```

## Deep Dive
Historically, case conversion has been around since early computing to harmonize text data processing. In Clojure, the `clojure.string/lower-case` function is part of the `clojure.string` library, a collection of utilities for string manipulation included in the core language.

Alternatives to `clojure.string/lower-case` include rolling your own function through mapping with `char` manipulation, but this is reinventing the wheel when you have a built-in function that's optimized and well-tested.

Internally, `clojure.string/lower-case` hands off the heavy lifting to Java's own `toLowerCase` method, since Clojure runs on the Java Virtual Machine (JVM). This ensures high performance as it leverages Java's mature libraries.

## See Also
- Clojure's `clojure.string` API: https://clojuredocs.org/clojure.string
- Java's `String.toLowerCase()` method: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
