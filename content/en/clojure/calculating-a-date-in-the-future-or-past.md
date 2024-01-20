---
title:                "Calculating a date in the future or past"
html_title:           "Clojure recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Working out a future or past date involves adjusting a given date by a specific duration. Programmers do this to handle time-based tasks like scheduling, reminders, or historical data tracking.

## How to:

To calculate a future or past date in Clojure, we use the `plus` or `minus` methods from `clj-time` library. First, we need to add the dependency in the project.clj file:

```Clojure
(defproject future-date "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-time "0.15.2"]])
```

Then, we import it in our Clojure code:

```Clojure
(ns future-date.core
(:require [clj-time.core :as t]
          [clj-time.coerce :as c]))
```

To add days to the current date:

```Clojure
(defn future-date [days]
  (-> (t/now)
      (t/plus (t/days days))
      (c/to-local-date)))
```

To subtract days from the current date:

```Clojure
(defn past-date [days]
  (-> (t/now)
      (t/minus (t/days days))
      (c/to-local-date)))
```

Sample output:

```Clojure
(future-date 5)  
;; Output: 2022-03-25

(past-date 5)
;; Output: 2022-03-15
```

## Deep Dive

Dating back to the ‘70s, programmers calculated future or past dates using built-in date and time functionalities in the programming languages or custom-built code. Now we use libraries providing relevant functions.

Alternatives for `clj-time` include `java.time` in Java, `date-fns` in JavaScript, or `Joda-Time` in Java. All are worth considering if you're working with those languages.

The key implementation detail is that both `plus` and `minus` methods return a new ReadableInstant object, leaving your original date unaffected. Additionally, they can handle edge cases regarding different number of days in months and leap years.

## See Also

Check out these sources for further details:

- Clj-time GitHub repository: https://github.com/clj-time/clj-time
- Java 8 Date-Time guide: https://www.baeldung.com/java-8-date-time-intro
- Joda-Time library: https://www.joda.org/joda-time/