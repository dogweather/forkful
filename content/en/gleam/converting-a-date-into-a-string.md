---
title:                "Gleam recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in programming, especially when working with user inputs or API requests. Representing dates in a human-readable format is important for better understanding and communication.

## How To
To convert a date into a string in Gleam, we can use the `dates` module. Let's say we have a Unix timestamp of `1519713600` which represents March 1st, 2018. We can use the `format` function to convert it into a string in the format of our choice.

```Gleam
import gleam/dates

let timestamp = 1519713600
let date = dates.from_unix_time(timestamp)
let string = dates.format(date, "%B %d, %Y") // Output: March 01, 2018
```

We can also customize the format of the string by using the symbols defined in the `dates` module's documentation. For instance, if we want to display the time in 24-hour format, we can use the `%H:%M` symbol as shown below.

```Gleam
import gleam/dates

let timestamp = 1519713600
let date = dates.from_unix_time(timestamp)
let string = dates.format(date, "%B %d, %Y at %H:%M") // Output: March 01, 2018 at 00:00
```

## Deep Dive
Behind the scenes, the `dates` module uses the Erlang `calendar` module for date and time calculations. This allows Gleam to have efficient and accurate datetime handling. Additionally, the `dates` module also provides functions for converting from and to other popular date formats, such as ISO 8601 and RFC 3339.

It's important to note that Gleam represents dates and times in the UTC time zone. If you need to work with local time, you can use the `dates.Timezone` module to convert the UTC datetime to your desired timezone.

## See Also
- [Gleam dates module documentation](https://gleam.run/docs/std/dates)
- [Erlang calendar module documentation](http://erlang.org/doc/man/calendar.html)
- [ISO 8601 date and time format](https://en.wikipedia.org/wiki/ISO_8601)
- [RFC 3339 date and time format](https://www.ietf.org/rfc/rfc3339.txt)