---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:35:15.946811-07:00
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means converting human-readable date text into a format the computer understands. Programmers do it because computers prefer dates as numbers for sorting, storing, or manipulating.

## How to:

Clojure leans on Java for date parsing, so we'll use `java.time.LocalDate` here:

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date "yyyy-MM-dd" date-str))

(println (parse-date "2023-04-05"))
```

Output:

```
#object[java.time.LocalDate 0x4b121a5e "2023-04-05"]
```

Here `java-time` is a Clojure library that wraps `java.time` APIs. It's more idiomatic Clojure than raw Java interop.

## Deep Dive

Clojure, born in 2007, is a modern Lisp that runs on the JVM. It provides interop with Java, including date handling. Before `java.time` (introduced in Java 8), Java used `java.util.Date` and `java.text.SimpleDateFormat`, clunky and less thread-safe.

`clj-time`, a Joda-Time wrapper, was popular for Clojure before `java-time`, but Joda-Time is now considered obsolete. Nowadays, `java-time` is the go-to since it wraps around the `java.time` package, which is far superior and immutable by default.

There are pure Clojure libraries too, like `tick`, but they also build on top of Java's `java.time` for practical reasons. The underlying `java.time` package uses the ISO calendar system but supports others. Such flexibility means that Clojure programs aren't just JVM-friendly but also internationally ready.

## See Also

- [Clojure Docs](https://clojure.org/)
- [java-time library](https://github.com/dm3/clojure.java-time)
- [Older clj-time library](https://github.com/clj-time/clj-time)
- [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)

Keep exploring and happy coding!
