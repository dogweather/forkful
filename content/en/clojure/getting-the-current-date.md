---
title:    "Clojure recipe: Getting the current date"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
As a programmer, there are many scenarios in which you may need to include the current date into your code. This could be for timestamping logs, scheduling tasks, or simply displaying the date in a user interface. Whatever the reason may be, being able to get the current date using Clojure can come in handy.

## How To

In Clojure, there are multiple ways to get the current date depending on your needs. The most straightforward way is to use the `java.util.Date` class and the `new` function to create a new instance. This can then be printed out using the `println` function to display the current date and time.

```Clojure
(def current-date (java.util.Date.))
(println current-date)
```

Output: 
`Tue Mar 23 13:15:28 GMT 2021`

Additionally, if you want the date in a specific format, you can use the `clj-time` library which provides various functions for manipulating dates and times. The `now` function returns the current date and time in a `DateTime` object which can be formatted using the `format` function.

```Clojure
(require '[clj-time.core :as time])

(def current-date-time (time/now))
(println (time/format "dd MMM yyyy HH:mm:ss" current-date-time))
```

Output:
`23 Mar 2021 13:15:58`

## Deep Dive
As mentioned, the `clj-time` library offers more functionality for working with dates and times in Clojure. It has functions for converting between time zones, calculating durations, and comparing dates. The library also supports parsing and formatting dates based on user-defined patterns.

Clojure also has the `java.time` API introduced in Java 8, which can be used for more advanced date and time operations. This offers a modern and more intuitive way of working with dates and times in Clojure. However, it requires Java 8 or higher to be installed.

## See Also
- [JavaDocs for java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentation for clj-time library](https://cljdoc.org/d/clj-time/clj-time/0.15.2)
- [Explanation of Java 8's java.time API](https://docs.oracle.com/javase/tutorial/datetime/iso/)

No conclusion needed, you now have the knowledge to easily get the current date in Clojure for your coding tasks. Happy coding!