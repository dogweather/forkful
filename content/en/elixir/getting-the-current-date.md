---
title:                "Elixir recipe: Getting the current date"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, working with dates and times is a common task. Whether you are building a scheduling application, calculating time differences, or simply displaying the current date and time to your users, knowing how to get the current date in Elixir is an important skill to have.

## How To

Getting the current date in Elixir is very straightforward. We can use the `Calendar` module to retrieve the current date and time. Let's take a look at a simple example:

```Elixir
current_date = Calendar.utc_now()
```

In this code snippet, we are using the `Calendar.utc_now()` function to get the current date and time in UTC format. If we were to print out the `current_date` variable, we would see something like this:

```Elixir
#⇒ {{2019, 12, 14}, {22, 25, 36}}
```

As you can see, the date and time are represented as a tuple with the format `{{year, month, day}, {hour, minute, second}}`.

We can also use the `Date` module to retrieve only the current date without the time component. Here's an example:

```Elixir
current_date = Date.utc_today()
```

If we were to print out the `current_date` variable, we would see something like this:

```Elixir
#⇒ {2019, 12, 14}
```

## Deep Dive

Under the hood, the `Calendar` and `Date` modules use the Erlang `:calendar` and `:calendar_now` functions to get the current date and time. These functions rely on the system's clock, so it is important to ensure that your system's clock is accurate.

It is also worth mentioning that the `Calendar` and `Date` modules allow us to convert the current date and time to different formats, such as local time or a specific time zone. This can be useful when working with global applications or when dealing with user-specific time zones.

## See Also

- [Elixir Calendar module documentation](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir Getting Started guide](https://elixir-lang.org/getting-started/introduction.html)

Now that you know how to get the current date in Elixir, you can confidently tackle any date-related tasks in your projects. Happy coding!