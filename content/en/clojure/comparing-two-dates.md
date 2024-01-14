---
title:                "Clojure recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

>


## Why

Have you ever needed to compare two dates in your Clojure program? Whether you're working with time-sensitive data or need to check for specific events, comparing dates can be crucial in many programming tasks. In this blog post, we will explore how to compare two dates in Clojure and dive deeper into the specifics of this useful function.

## How To

To compare two dates in Clojure, we will use the `clj-time` library. First, we need to import the library. We can do this by adding the following line at the top of our code:

```Clojure
(ns my-program
  (:require [clj-time.core :as t]))
```

Once we've imported the library, we can use the `t/compare` function to compare two dates. This function takes two arguments, in this case, the two dates we want to compare. For example, let's say we have two dates: `date1` and `date2`. We can use the function like this:

```Clojure
(t/compare date1 date2)
```

The `t/compare` function will return a value of `1` if `date1` is greater than `date2`, `-1` if `date2` is greater than `date1`, or `0` if both dates are equal. Let's see this in action with some sample code:

```Clojure
(def date1 (t/date-time 2021 8 31))
(def date2 (t/date-time 2021 9 1))
(println (t/compare date1 date2))
```

The output of this code will be `-1` because `date2` is one day ahead of `date1`. You can try changing the dates to see how the output changes.

## Deep Dive

Now let's dive deeper into the `t/compare` function. This function uses the `java.util.Date` class under the hood, which has a built-in `compareTo` method. The `t/compare` function simply calls this method and converts the result into the values of `1`, `-1`, or `0`.

It's important to note that the `t/compare` function only compares the dates' positions on the timeline, not the actual values. This means that it will return the same result for `date1` and `date2` regardless of the time component. For example, if `date1` is `2021-09-01 00:00:00` and `date2` is `2021-09-01 23:59:59`, the `t/compare` function will still return `0` because they are on the same day.

## See Also

Here are some helpful links to learn more about comparing dates in Clojure:

- [clj-time documentation](https://github.com/clj-time/clj-ti