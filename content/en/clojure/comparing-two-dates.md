---
title:    "Clojure recipe: Comparing two dates"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Why

As a programmer, you may often need to compare dates in your code. This can be for a variety of reasons such as sorting data, checking for overlaps, or calculating time differences. Understanding how to compare dates in Clojure can save you time and headaches in your coding process.

# How To

In Clojure, dates are represented as the number of milliseconds since January 1, 1970. This means that comparing dates simply involves comparing their millisecond values. Let's take a look at some code examples to illustrate this.

```Clojure
(def date1 (date-time 2021 10 15))
(def date2 (date-time 2021 8 1))

(println (compare date1 date2))
```

This code creates two date objects, `date1` and `date2`, and uses the `compare` function to compare them. The output of this code is `1`, indicating that `date1` is later than `date2`. Now let's try to compare `date2` to `date1`:

```Clojure
(println (compare date2 date1))
```

The output of this code is `-1`, meaning that `date2` is earlier than `date1`. Clojure's `compare` function follows a convention where it returns `1` if the first argument is greater, `-1` if the second argument is greater, and `0` if they are equal.

We can also use the `before?` and `after?` functions to check if one date is before or after another.

```Clojure
(println (before? date1 date2))
(println (after? date1 date2))
```

The output of these two lines of code will be `false` and `true` respectively, indicating that `date1` is after `date2`.

# Deep Dive

It's important to note that when comparing dates, the time zone is taken into consideration. This means that if you compare two dates in different time zones, the result may not be what you expect. To avoid this, you can use the `java.time` library which allows you to specify a specific time zone for your dates.

Another consideration is that dates can also be compared using mathematical operators like `<` and `>`. However, this method can sometimes lead to unexpected results, especially when dealing with daylight savings time. It's recommended to stick to the `compare` function for accurate date comparisons.

# See Also

For more information on Clojure date and time management, check out the official documentation and these helpful resources:

- Official Clojure Documentation: https://clojure.org/guides/learn/functional_programming#_date_and_time
- Clojure Date and Time Library: https://clj-time.github.io/clj-time/index.html
- Clojure Date Cheat Sheet: https://clojure.org/api/cheatsheet#date_and_time_functions