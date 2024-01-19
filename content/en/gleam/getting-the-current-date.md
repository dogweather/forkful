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

## What & Why? 
Grabbing the current date in your program is exactly what it sounds like — getting today's date. This lets you timestamp data, sort by recent activity, and schedule things.

## How to:
Gleam doesn't have built-in methods for date & time, but you can use Erlang's builtin `:calendar` module.

```Gleam
import erlang 

fn get_current_date() {
   let { date, _ } = erlang.time()
   date
}
```
This will return a tuple like: {year, month, day} 

## Deep Dive
Historically, Gleam hasn't included date and time libraries due to its focus on type safety. It relies heavily on Erlang's `:calendar` module for date and time functionality, making it directly accessible via Erlang interoperability. 

If not comfortable with `:calendar`, you can check out third-party libs like 'gleam-calendar' or 'gleam_datetime'.

While getting the current date may seem straightforward, what's actually happening under the hood is our program asking the system clock of the OS for the current date, which is then translated into a format that's useful for the program.

## See Also
Check these for more scoop on Date & Time handling in Gleam:
1. `:calendar` docs: https://erlang.org/doc/man/calendar.html
2. 'gleam-calendar': https://hex.pm/packages/gleam_calendar
3. 'gleam_datetime': https://hex.pm/packages/gleam_datetime