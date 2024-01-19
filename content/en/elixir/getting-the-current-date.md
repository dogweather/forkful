---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Get Current Date in Elixir: Riding the Time Machine

## What & Why?

Getting the current date is simply retrieving the present day’s information (year, month, day) in the system timezone. Coders do it when building features such as event logging, time tracking, or date stamping records.

## How to:

In Elixir, you use the `DateTime` module to get the current date. Here's how you do it:

```elixir
DateTime.utc_now() |> DateTime.to_date()
```

When you run it, you can expect to see something like this:

```elixir
~D[2022-03-17]
```

This returns the current UTC date.

## Deep Dive 

Elixir's `DateTime` was introduced in version 1.3 as part of the standard library. Before that, developers used libraries like Timex. However, the introduction of `DateTime` as a built-in Elixir module made handling date and time simpler and more straightforward.

Despite `DateTime`'s functionality ease, you might want to use other ways to get the current time. For instance, `NaiveDateTime.local_now/0` gives local date and time without timezone info. 

Your choice depends on whether your application needs to account for time zones. But remember: it's usually best to use UTC date and handle timezone conversions separately.

Another aspect to consider is system clock skews, which might give imprecise results. However, this is a larger timekeeping issue not specific to Elixir programming.

## See Also

To learn more about working with dates and times in Elixir, check out these resources:

- Elixir's `DateTime` Official Docs: https://hexdocs.pm/elixir/DateTime.html
- Tutorial on Elixir's date, time and timestamps: https://www.mitchellhanberg.com/posts/2018/07/31/elixir-dates-times-and-timestamps.html
- Why timezones, leap seconds matter: https://zachholman.com/talk/utc-is-enough-for-everyone-right