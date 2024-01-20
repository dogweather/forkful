---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is testing their chronological order or difference. This is crucial in programming for tasks like scheduling, determining durations, and time-based logging.

## How to:

Let's get hands-on with breaking dates into tuples and comparing them using Gleam.

```Gleam
pub fn compare_dates(date1: tuple(Int, Int, Int), date2: tuple(Int, Int, Int)) {
  case(date1, date2) {
    (
      tuple(year1, month1, day1),
      tuple(year2, month2, day2),
    ) ->
      case(year1 <=> year2, month1 <=> month2, day1 <=> day2) {
        (Equal, Equal, Equal) -> "Dates are equal"
        (Larger, _, _) -> "First date is later"
        (_, Larger, _) -> "First date is later"
        (_, _, Larger) -> "First date is later"
        _ -> "Second date is later"
      }
  }
}

fn main() {
  let date1 = tuple(2021, 10, 20);
  let date2 = tuple(2023, 11, 22);
  compare_dates(date1, date2)
  |> io.println("Comparison result: ")
}
``` 

Executing the above code will output:

```Gleam
Comparison result: Second date is later
```

## Deep Dive

Historically, comparing two dates was a more complex task due to differences in calendar systems and handling of leap years. Modern programming languages have robust date libraries that handle these complications.

An alternative to our approach can be using a date library that provides a native comparison function. Some programming languages provide native support for date comparison, while others may require importing a date library.

Our implementation detail is converting each date to a tuple and comparing their year, month, and day segments. This is a simple way to break down the problem of date comparison. However, this version doesn't account for leap years and is limited to the Gregorian calendar.

## See Also

1. For a deep understanding of date-related issues in Gleam, refer to the [Gleam documentation](https://gleam.run/documentation/).
4. For an understanding of different calendar systems, this [article](https://www.timeanddate.com/calendar/) offers a thorough explanation.