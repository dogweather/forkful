---
title:                "Converting a date into a string"
html_title:           "Clojure recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in many programming languages, and Clojure is no exception. By converting a date into a string, you can easily format it for display, storage, or comparison with other dates. This can be especially useful when working with time-sensitive data, such as in finance or project management.

## How To
To convert a date into a string in Clojure, you can use the `format` function from the `clojure.string` library. It takes in a string as a template and a date as an argument, and returns a string representing the date in the specified format. Let's take a look at an example:

```Clojure
(require '[clojure.string :as str])

(def date (java.util.Date.)) ; create a new Date object representing current date and time
(str/format "The date today is %d/%m/%Y" date)
```

In this example, we use the `%d` placeholder to indicate where we want the day of the month to be displayed, the `%m` placeholder for the month, and the `%Y` placeholder for the year. The output will be something like: `The date today is 21/10/2021`.

You can also use the `format` function with any date that can be converted to a `java.util.Date` object, such as using the `new` function with a specific date:

```Clojure
(def christmas (java.util.Date. 2021 12 25)) ; create a date for Christmas 2021
(str/format "Merry Christmas! Today is %A, %d/%m/%Y" christmas)
```

The output will be: `Merry Christmas! Today is Saturday, 25/12/2021`.

## Deep Dive
Under the hood, the `format` function uses the `SimpleDateFormat` class from Java to perform the conversion. This class allows for a wide variety of formatting options, including the ability to specify different languages and locales. You can check out the official Java documentation for `SimpleDateFormat` to learn more about its syntax and options.

It's also worth noting that the `clojure.string` library contains other useful functions for manipulating and formatting strings, such as `join`, `split`, and `replace`. These functions can come in handy when working with dates, as you may need to manipulate the string output further to fit your specific needs.

## See Also
- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Clojure.string Library Documentation](https://clojure.github.io/clojure/clojure.string-api.html)