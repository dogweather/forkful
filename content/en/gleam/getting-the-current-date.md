---
title:                "Gleam recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to get the current date in your Gleam program? Maybe you're building an appointment scheduling system or displaying the date on a website. Whatever the case may be, knowing how to get the current date is a useful skill to have in your programming toolkit.

## How To

Getting the current date in Gleam is actually quite simple. First, we need to import the Date module from the standard library:

```
Gleam import Date
```

Next, we can use the `Date.now` function to get the current date and time:

```
Gleam current_date = Date.now
```

This will return a `Date.DateTime` record, which contains fields for the month, day, year, hour, minute, second, and millisecond. We can access these fields like so:

```
Gleam month = current_date.month
Gleam year = current_date.year
```

We can also format the date to our desired format using the `Date.format` function. For example, if we wanted to display the date in the format of "Month Day, Year", we could do the following:

```
Gleam formatted_date = Date.format(current_date, "%B %e, %Y")
```

This will return a formatted string such as "January 1, 2021". You can experiment with different formatting options to get the date displayed exactly how you want it.

## Deep Dive

Under the hood, the `Date.now` function uses the Erlang `os:system_time` function to get the current system time in milliseconds. It then converts that millisecond value to a `Date.DateTime` record using some built-in Gleam functions.

Additionally, the `Date.format` function uses formatting codes that are similar to the ones used in the C programming language's `strftime` function. So, if you're familiar with C, this should look familiar to you.

## See Also

Here are some helpful links for further reading on Gleam's Date module:

- [Gleam Date Module Documentation](https://gleam.run/modules/date.html)
- [Erlang os:system_time function documentation](http://erlang.org/doc/man/os.html#system_time-0)
- [C strftime function documentation](https://en.cppreference.com/w/cpp/chrono/c/strftime)