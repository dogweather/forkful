---
title:                "Clojure recipe: Calculating a date in the future or past"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming projects, and Clojure offers a simple yet powerful way to do so. Whether you need to schedule tasks, determine deadlines, or track historical events, being able to accurately calculate dates is an important skill for any Clojure programmer.

## How To

Calculating a date in the future or past involves using the `#inst` reader macro and the `days` and `+` functions. Let's look at some examples:

```
(def today #inst "2021-01-01")
=> #inst "2021-01-01"

(+ today (days 7))
=> #inst "2021-01-08"

(+ today (days -14))
=> #inst "2020-12-18"

```

In the first example, we define the `today` variable as an `#inst` value representing January 1st, 2021. We then add 7 days to it using the `days` function and the `+` function, which results in the date for one week later, January 8th, 2021.

In the second example, we use a negative value for the `days` function to subtract 14 days from the `today` variable, resulting in a date for two weeks prior, December 18th, 2020.

You can also use the `weeks`, `months`, and `years` functions in conjunction with the `days` function to calculate dates in larger increments. For example:

```
(def today #inst "2021-01-01")
=> #inst "2021-01-01"

(+ today
   (weeks 2)
   (years -1))
=> #inst "2020-01-15"

```
This example uses the `+` function to add 2 weeks to `today` and then subtracts 1 year from that result, resulting in a date for January 15th, 2020.

## Deep Dive

Behind the scenes, Clojure uses Java's `java.util.Calendar` class to perform these date calculations. The `#inst` reader macro, when followed by a string in the format of "yyyy-mm-dd", creates an instance of `java.util.Date`, which is then converted to a `Calendar` object using the `calendar` function. This object is then used in conjunction with the `days`, `weeks`, `months`, and `years` functions to calculate dates.

It is important to note that the `days`, `weeks`, `months`, and `years` functions all take into account the varying number of days in each month and the leap year rule, ensuring accurate date calculations.

## See Also

To learn more about date calculations and manipulation in Clojure, check out these resources:

- Official Clojure Documentation for `#inst`: https://clojure.org/reference/reader#inst
- JavaDocs for `java.util.Calendar`: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Clojure Date and Time Library: https://github.com/clj-time/clj-time