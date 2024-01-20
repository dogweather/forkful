---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date and time is a common operation in programming. It is used for recording when certain events happen, for timestamping entries, and coordinating activities in real-time apps.

## How to?

In Clojure, the Java interop functionality gives us access to date-time libraries of Java. Here's how to get the current date:

```Clojure
(import 'java.util.Date)
(def current-date (Date.))

;; Let's print the date
(println current-date)
```

This will output the current date and time, something like:

```Clojure
Wed Sep 01 17:29:56 BST 2021
```

In addition, the `clj-time` library, a Clojure simple wrapper of the Joda-Time library, is often used:

```Clojure
(ns my-namespace
  (:require [clj-time.core :as t]))

;; Get current date-time
(def now (t/now))

;; Print the current date-time
(println now)
```

The output will be similar to:

```Clojure
2021-09-01T17:30:00.023Z
```

## Deep Dive

Clojure being a dialect of Lisp that runs on the Java Virtual Machine (JVM), uses the date-time library of Java. Historically, Java's old date and time classes (`java.util.Date` and `java.util.Calendar`) had many shortcomings which were addressed in java.time, part of Java 8 and later. 

In addition to the built-in Java libraries, many Clojure developers use the `clj-time` library, a wrapper around the Joda-Time library for handling dates and times. `clj-time` is popular due to its simplicity and its rich set of functionalities, and it fits more naturally into Clojure's functional programming style.

Java interop in Clojure is rather direct but can be verbose, and using Java classes directly can lead to unclear code. Therefore, it's often preferred to use Clojure libraries that wrap Java libraries and provide a more idiomatic Clojure interface.

## See Also

[1] [Clojure Date and Time - Baeldung](https://www.baeldung.com/clojure-date-time)

[2] [Working with date time in Clojure - adit.io](http://adit.io/posts/2012-04-24-working_with_dates_and_times_in_clojure.html)

[3] [clj-time Github page](https://github.com/clj-time/clj-time)

[4] [Official clojure.java-time docs](https://clojure.github.io/java-time/)