---
date: 2024-02-03 19:02:42.042436-07:00
description: "How to: Clojure, being a JVM language, allows you to use Java's date\
  \ and time libraries directly. Let's start with the built-in Java interoperation\
  \ and\u2026"
lastmod: '2024-03-13T22:44:59.754121-06:00'
model: gpt-4-0125-preview
summary: Clojure, being a JVM language, allows you to use Java's date and time libraries
  directly.
title: Parsing a date from a string
weight: 30
---

## How to:
Clojure, being a JVM language, allows you to use Java's date and time libraries directly. Let's start with the built-in Java interoperation and then explore how to utilize a popular third-party library, clj-time, for more idiomatic Clojure solutions.

### Using Java Interop
Clojure can directly leverage Java's `java.time.LocalDate` for parsing dates from strings:
```clojure
(require '[clojure.java.io :as io])

; Parsing a date using Java interop
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Output: 2023-04-01
```

### Using clj-time
A more idiomatic Clojure library for dealing with dates and times is `clj-time`. It wraps Joda-Time, a comprehensive library for date and time operations. First, you'll need to add `clj-time` to your project's dependencies. Here's how you parse a date string using `clj-time`:

```clojure
; Make sure to add [clj-time "0.15.2"] to your project.clj under :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Define a formatter
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Output: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

These examples demonstrate basic date parsing. Both methods are useful, but `clj-time` can provide a more Clojure-centric approach with additional functionalities for complex requirements.
