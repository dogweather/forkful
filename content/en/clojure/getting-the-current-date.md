---
title:    "Clojure recipe: Getting the current date"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why 

In today's digital world, the accurate and current date is an essential piece of information for many programming tasks. Whether it's for timestamping data, scheduling tasks, or creating time-based logic, being able to easily access the current date is a crucial skill for any Clojure programmer.

## How To

To get the current date in Clojure, we can use the `clojure.java-time` library, specifically the `java.time.LocalDate` class. Let's take a look at a simple example:

```
(ns blog.core
  (:require [clojure.java-time :as t]))

(t/local-date)
```

The `t/local-date` function returns an instance of `java.time.LocalDate` representing today's date. We can also specify a time zone if needed, for example:

```
(t/local-date t/-zone-id ise-eastern)
```

This will give us the current date in the Eastern Time Zone. Now, let's say we want to format the date in a specific way, such as "MM/dd/yyyy". We can accomplish this using the `t/format` function:

```
(t/format (t/local-date) "MM/dd/yyyy")
=> "05/21/2020"
```

We can also extract specific information from the current date using the `with` function. For example, if we want to get the day of the week:

```
(t/with (t/day-of-week (t/local-date)) t/translated-name)
=> "Thursday"
```

## Deep Dive 

The `clojure.java-time` library is built on top of Java's `java.time` API, which provides extensive functionality for working with dates and times. This means that you can use all the features and methods available in Java's API while working with dates in Clojure.

Additionally, the `java.time` API is designed to be immutable and thread-safe, making it easy to work with in a functional language like Clojure. This also means that there are no hidden side-effects when working with dates, making debugging and testing much easier.

It's worth mentioning that the `clojure.java-time` library is not the only option for working with dates in Clojure. There are other libraries, such as `clj-time` and `tick`, that also provide date and time functionality. It's always a good idea to research and compare different options before deciding on which library to use in your project.

## See Also 

- [The official clojure.java-time documentation](https://clojuredocs.org/clojure.java-time)
- [Java's java.time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [An in-depth tutorial on working with dates in Clojure](https://lambdaisland.com/guides/clojure-dates)

By using `clojure.java-time`, we can easily access and manipulate the current date in our Clojure projects. With a wide range of features and flexibility, this library is a valuable tool for any Clojure programmer.