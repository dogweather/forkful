---
title:                "Comparing two dates"
html_title:           "Gleam recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a commonly used operation in programming where the dates are compared and the results are evaluated to check which one is earlier or later. This is often done to compare the dates of events, appointments, or deadlines. Programmers use this to efficiently organize tasks and prioritize them based on their date.

## How to:

To compare two dates in Gleam, we can use the `Date.compare/2` function. This function takes two dates as arguments and returns `-1` if the first date is earlier, `1` if the second date is earlier, or `0` if they are equal. Let's see some examples:

```Gleam
let first_date = Date.from_tuple((2021, 9, 1))
let second_date = Date.from_tuple((2021, 9, 15))

Date.compare(first_date, second_date)
// Output: -1
```

In the above example, we compared two dates using the `Date.compare/2` function and got the expected output of `-1` indicating that the first date is earlier than the second date. Let's see another example:

```Gleam
let task1 = {
    title: "Meeting with team",
    date: Date.from_tuple((2021, 9, 10))
}

let task2 = {
    title: "Submit project proposal",
    date: Date.from_tuple((2021, 9, 1))
}

Date.compare(task1.date, task2.date)
// Output: 1
```

In this example, we compared the dates of two tasks and got the output `1` indicating that task1 needs to be done before task2 as it has an earlier date.

## Deep Dive

In the past, comparing dates was more complex and required more coding as dates were stored in various formats such as Year-Month-Day and Day-Month-Year. With the standardization of the ISO 8601 date format (YYYY-MM-DD), comparing dates has become easier and more efficient.

In addition to the `Date.compare/2` function, there are other alternatives for comparing dates in Gleam such as using the `timer:compare/2` function from the `gleam_timer` package. However, the `Date.compare/2` function is specifically designed for comparing dates and is recommended for this purpose.

The implementation details of the `Date.compare/2` function may vary depending on the platform it is used on. For example, on the BEAM (Erlang Virtual Machine), the function is implemented using the `calendar:compare_days/2` function.

## See Also

- Official Gleam documentation: [Date module](https://gleam.run/documentation/std/date#compare) 
- ISO 8601 date format: [ISO 8601 - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- Alternative date comparison function: [gleam_timer package](https://gleam.run/packages/gleam_timer/latest/)