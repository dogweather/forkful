---
title:                "Getting the current date"
date:                  2024-01-20T15:13:50.618682-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Clojure means fetching the present calendar date your program is running on. Programmers snag the date to timestamp events, cache expiries, or for any time-sensitive features.

## How to:

```Clojure
;; Importing Java interop to use Date classes
(import java.util.Date)
(import java.text.SimpleDateFormat)

;; Getting the Current Date and Time
(defn current-date-time []
  (let [today (new Date)]
    (println "Current date and time: " today)))

(current-date-time)
;; Output: Current date and time:  Wed Apr 05 00:12:35 BST 2023

;; Formatting Date to a Specific Pattern
(defn formatted-current-date []
  (let [today (new Date)
        formatter (SimpleDateFormat. "dd-MM-yyyy")]
    (println "Today's date is: " (.format formatter today))))

(formatted-current-date)
;; Output: Today's date is:  05-04-2023
```

## Deep Dive

Clojure, a modern dialect of Lisp, offers Java interoperability, so we often use Java's rich Date and Time API. Historically, dates were handled pretty differently – think cogs and sun dials – but in programming, we had Java's `Date` and `Calendar` as early as JDK 1.0. Now, we also have `java.time` from Java 8 for a more comprehensive and unified approach to temporal data. 

While `java.util.Date` serves well for basic needs, it has its quirks, like being mutable – meaning it can change after creation, e.g., with `setTime`. `java.time` is immutable and more versatile, but for simple tasks like grabbing the current date, `Date` still does the trick.

Alternatives within Clojure include libraries like clj-time, which wraps Joda Time (a precursor to `java.time`), and tick, a modern Clojure library for dealing with time. Each has its pros and cons depending on scope and complexity of your time-handling needs.

Implementation-wise, fetching the current date-time is a straightforward affair in Clojure due to its Java roots. It's typically a one-liner, though date formatting requires a few more steps and understanding Java's date formatting patterns and standards. 

## See Also

Here are some nifty corners of the web for the curious Clojure time-traveler:

- Clojure Docs: https://clojuredocs.org/
- Java 8 Date/Time API: https://docs.oracle.com/javase/tutorial/datetime/
- clj-time GitHub repo: https://github.com/clj-time/clj-time
- tick GitHub repo: https://github.com/juxt/tick