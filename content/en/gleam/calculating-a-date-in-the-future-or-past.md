---
title:                "Calculating a date in the future or past"
html_title:           "Gleam recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past means determining a date from a specific time-reference, either ahead or behind. Programmers typically do so to manage scheduling tasks, timeouts, reminders, and such time-dependent logic.

## How to:

Let's walk through an example where we add and subtract days from a given date.

```Gleam
import gleam/calendar.{Date, add_days, subtract_days}
import gleam/io.{println}

fn main() {
let start_date = Date.new(2023, 6, 30) //starting point 
let future_date = add_days(start_date, 5) //adding 5 days 
let past_date = subtract_days(start_date, 7) //subtracting 7 days 

println(future_date)
println(past_date)
}
```
Upon running this code, your output should be something like:

```Gleam
Date(2023, 7, 5)
Date(2023, 6, 23)
```
So, we see it's pretty straightforward to manipulate dates in Gleam.

## Deep Dive

Historically, dealing with dates and times has always been tricky due to varying calendars systems and time zones. Libraries like Gleam's `calendar` have become helpful in providing tidy approaches to these complexities.

There are, of course, alternate libraries one might consider, like `age`, but `calendar` comes built-in and offers a solid breadth of functionality.

When creating `add_days` or `subtract_days`, Gleam essentially converts the specific date into a form easier to perform calculations on (like Julian Day Number), performs the operation, and then converts it back.

## See Also

For deeper insights, consider the following:
- Official Gleam Docs: [Gleam’s `calendar` library](https://hexdocs.pm/gleam_stdlib/gleam/calendar.html)
- Age - Chronological Calculations: [`age` library](https://hex.pm/packages/age)

Do keep these bits handy, as you'll find yourself dealing with dates more often than you might think in your programmer's journey.