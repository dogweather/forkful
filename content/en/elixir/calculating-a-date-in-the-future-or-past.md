---
title:                "Elixir recipe: Calculating a date in the future or past"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 

In the world of programming, there are many instances where we may need to calculate dates in the future or past. Whether it's for scheduling tasks or managing events, being able to accurately calculate dates in Elixir can be incredibly useful.

## How To 

To calculate a date in the future or past in Elixir, we can use the `DateTime` module. Let's take a look at some examples: 

```
Elixir date_in_future = DateTime.utc_now() |> DateTime.add(60, :days) 
Elixir date_in_past = DateTime.utc_now() |> DateTime.add(-14, :hours)
```

In the first example, we are calculating a date 60 days in the future. We first call the `DateTime.utc_now()` function to get the current date and time in UTC format. Then, we use the `DateTime.add()` function to add 60 days to the current date. Similarly, in the second example, we are calculating a date 14 hours in the past.

We can also use the `DateTime.add()` function to calculate dates based on different units such as weeks, months, or years. For example: 

```
Elixir date_in_future = DateTime.utc_now() |> DateTime.add(2, :years) 
```

This will return a date 2 years in the future. We can also specify the unit as a keyword instead of an atom, for example: 

```
Elixir date_in_future = DateTime.utc_now() |> DateTime.add(1, :month) 
```

These examples may seem simple, but the `DateTime` module has many more functions and options for calculating dates. Keep reading to learn more!

## Deep Dive 

The `DateTime` module in Elixir provides a lot of useful functions for calculating dates. Here are a few of the most commonly used functions: 

- `DateTime.utc_now()` - returns the current date and time in UTC format 
- `DateTime.now()` - returns the current date and time in local time zone format 
- `DateTime.add(date, number, unit)` - adds the specified number of units to the given date 
- `Date.day_of_week(date)` - returns the day of the week for the given date 
- `Date.day_of_year(date)` - returns the day of the year for the given date 

These are just a few examples, but there are many more functions available in the `DateTime` module. It's important to note that all date calculations in Elixir are done using the Gregorian calendar.

## See Also 

- [Elixir Date and Time Calculation Documentation](https://hexdocs.pm/elixir/1.12.3/DateTime.html)
- [Elixir Date and Time Functions Cheat Sheet](https://devhints.io/elixir-date)

Calculating dates in the future or past may seem like a small task, but it can greatly improve the functionality of your Elixir programs. With the `DateTime` module, you have a powerful tool at your disposal. Happy coding!