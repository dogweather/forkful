---
title:                "Clojure recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
In any programming language, it is often necessary to convert dates into strings in order to display them in a more readable format. This is particularly relevant in user-facing applications where the date needs to be easily understood by a human.

## How To
Converting a date into a string in Clojure is a relatively straightforward process. The `format` function can be used to specify the desired format and convert the date into a string. Let's take a look at an example:

```Clojure
(format "dd/MM/yyyy" (java.util.Date.))
```

This code will return the current date in the format of day/month/year, for example, 06/12/2020. 

If you want to specify a different date, you can also use the `date-time` function, which takes in individual components of the date such as year, month, and day. Here's an example:

```Clojure
(format "y-MM-dd" (date-time 2020 12 6))
```

This code will return the same date but in the format of year-month-day, for example, 2020-12-06.

Another useful function for date formatting is the `date` function, which takes in a long value representing the number of milliseconds since January 1, 1970, and returns a date in that format. Here's an example:

```Clojure
(format "E, MMM d, yyyy" (date 1607223820000))
```

This code will return the date December 6, 2020 in the format of day of week, month, day, and year, for example, Sun, Dec 6, 2020.

## Deep Dive
In Clojure, dates are represented as Java objects of the java.util.Date class. This means that you can also use any Java libraries or methods for date formatting and manipulation in Clojure.

It is also important to consider time zones when converting dates into strings. Clojure offers the `clj-time` library, which provides functions for working with time zones and handling daylight saving time.

When formatting dates, it is important to pay attention to the symbols used. Some common symbols for formatting include `dd` for day, `MM` for month, `yyyy` for year, `E` for day of week, and `MMM` for month abbreviation.

## See Also
- [Clojure date-time library](https://github.com/clj-time/clj-time)
- [Java SimpleDateFormat class](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial on date formatting in Clojure](https://mindviewinc.com/Books/TIJ4/OnLineSupplements.jar/date-format.html)