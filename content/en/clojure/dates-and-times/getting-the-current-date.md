---
date: 2024-02-03 19:02:56.530577-07:00
description: 'How to: #.'
lastmod: '2024-03-13T22:44:59.755034-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Getting the current date
weight: 29
---

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
