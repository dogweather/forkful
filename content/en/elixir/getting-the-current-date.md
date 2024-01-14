---
title:    "Elixir recipe: Getting the current date"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
As an English reader, you may be wondering, why would I want to get the current date in an Elixir program? Well, there are a few reasons why you may want to do this. One common use case is to display the current date and time on a user interface, such as a website or mobile app. Another reason is for timestamping events or logging, to keep track of when specific actions or errors occur. Knowing the current date can also be helpful for scheduling tasks and setting reminders. 

## How To
To get the current date in Elixir, we can use the `:calendar.local_time` function, which returns a tuple containing the year, month, day, hour, minute, and second. We can then destructure this tuple to access individual components or use the `:calendar.strftime` function to format the date in a specific way. Let's take a look at some code examples to better understand this process:

```
# Get the current date as a tuple
{:ok, {year, month, day, hour, minute, second}} = :calendar.local_time()
# Output: {:ok, {2021, 9, 23, 14, 30, 0}}

# Destructure the tuple to access individual components
IO.puts("Current year: #{year}")
IO.puts("Current month: #{month}")
# Output: Current year: 2021
# Current month: 9

# Format the date using strftime
formatted_date = :calendar.strftime({year, month, day}, "%m/%d/%Y")
IO.puts("Formatted date: #{formatted_date}")
# Output: Formatted date: 09/23/2021
```

As you can see, we first call the `:calendar.local_time` function to get the current date as a tuple. Then, we can use pattern matching to destructure the tuple and access individual components. Alternatively, we can use the `:calendar.strftime` function to format the date in a specific way by passing in the desired format as a string. 

## Deep Dive
Now, let's take a deeper look into how Elixir handles dates and time. Elixir uses the Erlang standard library's `os:timestamp` function to retrieve the current time in UTC. This timestamp is then converted to a tuple containing all the necessary components for date and time calculations. The `:calendar` module provides functions for manipulating and formatting these tuples in various ways. However, it's important to note that Elixir does not handle time zones natively, so additional libraries may be needed for more complex operations in different time zones.

## See Also
- [Elixir Documentation: Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Erlang Documentation: Calendar](http://erlang.org/doc/man/calendar.html)
- [Moment: Timezone handling with Elixir](https://hexdocs.pm/moment/readme.html#timezone-handling-with-elixir)