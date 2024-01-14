---
title:                "Clojure recipe: Converting a date into a string"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 

Converting a date into a string is a common task in programming, especially when working with dates and timezones. By converting a date into a string, we can easily display it in a human-readable format or use it in different operations such as comparing dates or performing calculations.

## How To

To convert a date into a string in Clojure, we can use the `str` function and the `clj-time` library. First, let's require the library and create a date object using the `now` function:

```Clojure
(require '[clj-time.core :as time])

(def date (time/now))
```

Next, we can use the `format` function from the library to specify the output format we want for the date. For example, if we want the date to be displayed in the format "dd/MM/yyyy", we can use the following code:

```Clojure
(time/format date "dd/MM/yyyy")
```

This will return a string with the current date in that format, for example: "11/10/2021".

We can also specify a timezone for the date by using the `at-time-zone` function. For example, if we want to convert the date into a string in the "Etc/GMT+8" timezone, we would use the following code:

```Clojure
(time/format (time/at-time-zone date "Etc/GMT+8") "dd/MM/yyyy")
```

This will return the date in the specified timezone. It is important to note that when converting a date into a string, the output will always be a string, even if the input date is a `DateTime` object.

## Deep Dive

The `format` function from the `clj-time` library uses the `DateTimeFormatter` class under the hood. This class provides many options for formatting dates and times, including specifying the language, timezones, and patterns for displaying the date. You can check out the full documentation for `DateTimeFormatter` on the [Clojure website](https://cljdoc.org/d/clj-time/clj-time/0.15.2/doc/clj-time.format/DateTimeFormatter).

Additionally, the `str` function used in our code block is a built-in function in Clojure that is used to concatenate strings. This means we can also use it to convert other data types into strings, such as numbers or boolean values.

## See Also

- [Clj-time documentation](https://github.com/clj-time/clj-time)
- [Clojure formatting strings documentation](https://clojure.org/guides/learn/language/strings#_formatting_strings)
- [Java DateTimeFormatter class documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)