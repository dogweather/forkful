---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string means representing a date as a readable text. Programmers do this to enable user-friendly display of dates and enhance portability across different systems.

## How to:

Clojure provides the `clj-time` library to handle date-related operations. Suppose we’re given a date-time object and we want to convert it into a string:

```clojure
(require '[clj-time.format :as f])

(def date (t/date-time 2020 1 1))

(defn date-to-string [date]
  (f/unparse 
    (f/formatter "dd-MM-yyyy") 
    date))
    
(println (date-to-string date)) ; "01-01-2020"
```

## Deep Dive

Historically, date-time manipulation in Clojure wasn't as simple, but with Java interop and libraries like `clj-time`, things have become easier. 

A key alternative to `clj-time` is the `java.time` package, introduced in Java 8, which has rich features and is recommended for use in newer Java projects. 

As for the implementation, `clj-time` offers `formatter` function to define date format and `unparse` function to convert the date into the desired format.

For instance, if you want the output in the "MM-dd-yyyy" format:

```clojure
(defn date-to-string-alternative-format [date]
  (f/unparse 
    (f/formatter "MM-dd-yyyy") 
    date))
    
(println (date-to-string-alternative-format date)) ; "01-01-2020"
```

## See Also

- [Official Clojure Documentation](https://clojure.org/)
- [clj-time GitHub](https://github.com/clj-time/clj-time)
- `java.time` [API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) and [tutorial](https://www.baeldung.com/java-8-date-time-intro) on Baeldung.