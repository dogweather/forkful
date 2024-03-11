---
date: 2024-01-20 17:28:31.670616-07:00
description: "Calculating future or past dates involves manipulating dates to find\
  \ out what they'll be after a certain period or what they were. Programmers do this\
  \ for\u2026"
lastmod: '2024-03-11T00:14:33.610011-06:00'
model: gpt-4-1106-preview
summary: "Calculating future or past dates involves manipulating dates to find out\
  \ what they'll be after a certain period or what they were. Programmers do this\
  \ for\u2026"
title: Calculating a date in the future or past
---

{{< edit_this_page >}}

## What & Why?

Calculating future or past dates involves manipulating dates to find out what they'll be after a certain period or what they were. Programmers do this for things like scheduling events, reminders, or figuring out expiration dates.

## How to:

In Clojure, you'll mainly use the `clj-time` library for date operations. Here's a quick show:

```clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.periodic :as periodic])

;; Add 5 days to the current date
(let [now (time/now)
      five-days (time/plus now (time/days 5))]
  (str "Five days from now: " (coerce/to-string five-days)))

;; Subtract 10 days from a specific date
(let [specific-date (coerce/to-date-time "2023-03-01T12:00:00.000Z")
      ten-days-ago (time/minus specific-date (time/days 10))]
  (str "Ten days before March 1, 2023: " (coerce/to-string ten-days-ago)))
```

Sample output:
```
"Five days from now: 2023-03-23T08:00:00.000Z"
"Ten days before March 1, 2023: 2023-02-19T12:00:00.000Z"
```

## Deep Dive

In the earlier days, Coders used Java's `Date` and `Calendar` classes. But, let's be honest, they're a headache—verbose and error-prone. The `clj-time` library brought some sanity, wrapping Joda-Time's more developer-friendly API.

Alternatives? Java 8 introduced `java.time` (JSR-310), which is quite good, but in Clojure's neck of the woods, we're still cozy with `clj-time`.

When calculating dates, you use periods for concepts like "days" and "months" and durations for precise millisecond counts. Keep in mind time zones—dates and times can shift dramatically depending on the time zone rules, and daylight saving time (DST) can throw a spanner in your works.

## See Also

- `clj-time` GitHub repo: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Clojure’s `java-time`: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
