---
title:    "Clojure recipe: Calculating a date in the future or past"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming projects. This can be useful for scheduling tasks, setting reminders, or simply managing time-based data. In this blog post, we will explore how to use Clojure to easily calculate dates in the future or past.

## How To

To calculate a date in Clojure, we can use the `clj-time` library. First, we need to add the `clj-time` dependency to our project. In our `project.clj` file, we can add the following line under the `:dependencies` section:

```
[clj-time "0.15.2"]
```

Next, we need to import the necessary namespace in our Clojure file:

```
(ns your-project-name
  (:require [clj-time.core :as t]))
```

Now, let's explore how to calculate dates in the future or past using the following examples:

### Calculate date in the future

We can use the `t/plus` function to calculate a date in the future. This function takes in a time period and a starting date, and returns the calculated date.

```
(t/plus (t/months 1) (t/today))
;; Output: #object[org.joda.time.DateTime 0x149b8014 "2021-08-31T00:00:00.000Z"]
```

In this example, we are calculating the date one month from today.

### Calculate date in the past

Similarly, we can use the `t/minus` function to calculate a date in the past. This function also takes in a time period and a starting date, and returns the calculated date.

```
(t/minus (t/days 7) (t/today))
;; Output: #object[org.joda.time.DateTime 0x1c661cc "2021-07-25T00:00:00.000Z"]
```

In this example, we are calculating the date seven days before today.

### Calculate date with specific time

If we want to calculate a date with a specific time, we can use the `t/with-time` function. This function takes in a time and a date, and returns a new date with the specified time.

```
(t/with-time (t/time 12 0) (t/today))
;; Output: #object[org.joda.time.DateTime 0x6d6d805c "2021-07-31T12:00:00.000Z"]
```

In this example, we are calculating a date at 12PM today.

## Deep Dive

The `clj-time` library also provides functions for advanced date calculations such as daylight savings, leap years, and time zones. For a complete guide, please refer to the official documentation [here](https://github.com/clj-time/clj-time).

See Also

- [Official `clj-time` documentation](https://github.com/clj-time/clj-time)
- [ClojureDocs for `clj-time`](https://clojuredocs.org/clj-time)
- [A basic introduction to calculating dates in Clojure](https://scotthaleen.github.io/blog/2013/10/06/calculating-dates-in-clojure/)