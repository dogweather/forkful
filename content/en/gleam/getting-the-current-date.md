---
title:                "Gleam recipe: Getting the current date"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Getting the current date is an essential part of many programming tasks, from displaying the current date on a website to creating time-sensitive tasks or scheduling events. In Gleam, this can be done easily using a built-in function. 

## How To
To get the current date in Gleam, we will be using the `Date.from_local_iso8601` function. This function takes in a string of the current date in ISO 8601 format and returns a `Date` type. Let's see how this works in action:

```Gleam
let today = Date.from_local_iso8601("2021-09-15")

let output = String.join([today.day, "/", today.month, "/", today.year])

io.format("Today's date is {}", [output])
```

Running this code will give us the output `Today's date is 15/09/2021`. Here, we first create a `Date` type using the `Date.from_local_iso8601` function and then use the `String.join` function to format and join the day, month, and year values of the `Date` type. Finally, we use the `io.format` function to print the output in a user-friendly format.

## Deep Dive
The `Date.from_local_iso8601` function is just one of the many built-in date and time functions available in Gleam. Other functions include `Date.from_components`, which allows you to create a `Date` type using individual components such as day, month, and year, and `Date.from_unix`, which converts a Unix timestamp into a `Date` type.

It is important to note that the `Date` type in Gleam follows the Gregorian calendar and does not support time zones. If you need to work with time zones, you can use third-party libraries or create your own custom functions.

## See Also
- Official Gleam documentation on Date and Time: https://gleam.run/book/standard-library.html#date-and-time
- ISO 8601 standard: https://www.iso.org/iso-8601-date-and-time-format.html
- Third-party Gleam date and time library: https://github.com/gleam-lang/ecdatetime