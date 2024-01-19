---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing a Date from a String in Clojure

## What & Why?

Parsing a date from a string is the process of converting a string representation of a date and/or time into a usable date object in your program. We do this when we need to manage, format, or perform operations with dates that have been input or received as strings.

## How to:

Parsing a date from a string in Clojure can be smoothly done using the `clj-time` library. Here's a simple example:

```Clojure
(require '[clj-time.format :as f])

(defn parse-date [date-str]
  (f/parse (f/formatter "yyyy-MM-dd") date-str))

(parse-date "2022-07-01")
```

If you run the code above you can parse the date in a "yyyy-MM-dd" format. The output will look like this:

```Clojure
#object[org.joda.time.DateTime 0x643b6439 "2022-07-01T00:00:00.000Z"]
```

This is a DateTime object, and you can perform various operations on it.

## Deep Dive

Historically, Clojure relied on Java Interoperability for date parsing. This process was often seen as cumbersome. `clj-time`, a library built on Joda-Time, offers an idiomatic way to deal with dates and times in Clojure.

As an alternative, you could directly use Java Interop for manipulating dates but it's often more involved. Here's a simple example:

```clojure
(.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") "2022-07-01")
```
But considering readability and simplicity, `clj-time` is recommended.

Internally, `clj-time` leverages powerful DateTime structures from Joda-Time providing facilities for parsing and formatting dates, and various other time calculations.

## See Also

For more about parsing dates, consider these additional resources:

1. Full documentation for the clj-time library can be found at: https://github.com/clj-time/clj-time.

2. For a deeper understanding of Clojure date operations and more, you might want to dig into Clojure for the Brave and True (https://www.braveclojure.com/).

3. Related date and time Java interoperability practices in Clojure: https://clojure.org/reference/java_interop.

Remember, parsing a date from a string is a basic, but essential skill in your Clojure toolbelt. Get comfortable with date-parsing, and your future Clojure selves will thank you.