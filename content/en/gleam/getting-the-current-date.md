---
title:                "Getting the current date"
html_title:           "Gleam recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Gleaming the Current Date: Why and How

## What & Why?

Getting the current date is a common task in programming, as it allows us to keep track of time and create timestamped data. By using Gleam's built-in Date module, we can easily retrieve the current date and time in any desired format. This helps programmers accurately record and manipulate data, making their code more efficient and reliable.

## How to:

To get the current date in Gleam, we will use the `Date.now` function from the Date module. This function returns a tuple containing the date and time in the ISO 8601 format, which is widely used in data storage and transfer. Let's take a look at a simple code example:

```Gleam
import Date

fn main() {
  current_date = Date.now()
  io.print(current_date)
}
```

This code will output the current date and time in the format: `{year-}{month}-{day}T{hour}:{minute}:{second}.000{timezone}`. For example, the output may look like this: `2020-09-09T13:27:35.000+0000`.

## Deep Dive

Historically, getting the current date in programming has been a more complicated task, often involving complex date libraries and manual calculations. However, with the advancement of programming languages and frameworks, it has become much simpler and more standardized.

Some alternatives to using Gleam's built-in Date module include using a third-party library, such as Moment.js or Day.js, or using the system's built-in functions to retrieve the current date. However, these options may require additional setup and can be less efficient than using Gleam's native Date functionality.

Internally, Gleam uses the Erlang standard library's `calendar` module for handling dates and times. This module provides functions for creating, converting, and manipulating dates and times in various formats.

## See Also

For more information on Gleam's built-in Date module and its functions, check out the official documentation. You can also explore the `calendar` module in the Erlang standard library for a deeper dive into handling dates and times in programming. Happy coding!