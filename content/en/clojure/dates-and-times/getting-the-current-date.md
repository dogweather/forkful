---
aliases:
- /en/clojure/getting-the-current-date/
date: 2024-02-03 19:02:56.530577-07:00
description: "Getting the current date in programming is crucial for a myriad of reasons,\
  \ including logging, timestamping events, and scheduling tasks. In Clojure, a\u2026"
lastmod: 2024-02-18 23:09:10.734266
model: gpt-4-0125-preview
summary: "Getting the current date in programming is crucial for a myriad of reasons,\
  \ including logging, timestamping events, and scheduling tasks. In Clojure, a\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in programming is crucial for a myriad of reasons, including logging, timestamping events, and scheduling tasks. In Clojure, a Lisp dialect on the JVM, this task leverages Java interop capabilities, allowing for straightforward access to the rich Java Date-Time API.

## How to:

### Using Java Interop
Clojure's seamless interoperability with Java allows you to tap into the Java Date-Time API directly. Here's how you can get the current date:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Sample output
(get-current-date) ; "2023-04-15"
```

### Using clj-time Library
For a more idiomatic Clojure solution, you might opt for the `clj-time` library, a wrapper around Joda-Time, though for most new projects, the built-in Java 8 Date-Time API is recommended. However, should you prefer or require `clj-time`:

First, add `clj-time` to your project dependencies. In your `project.clj`, include:

```clojure
[clj-time "0.15.2"]
```

Then, use it to get the current date:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Sample output
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Both methods provide quick, effective ways to get the current date in Clojure, leveraging the power of the underlying Java platform or the convenience of a Clojure-specific library.
