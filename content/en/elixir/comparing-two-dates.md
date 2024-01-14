---
title:                "Elixir recipe: Comparing two dates"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates is a common task in programming, especially in applications dealing with time and scheduling. It allows for determining the relationships between dates, such as if one date is before, after, or equal to another date. In Elixir, there are built-in functions that make date comparison quick and easy.

## How To

To compare two dates in Elixir, we can use the `:calendar.compare` function. This function takes two dates as arguments and returns an atom representing the relationship between the dates. Let's take a look at some code examples to see how this works.

```elixir
# Comparing two dates using the `:calendar.compare` function
date_1 = ~D[2021-01-01] # January 1st, 2021
date_2 = ~D[2021-02-15] # February 15th, 2021

:calendar.compare(date_1, date_2) # returns :lt (less than)
```

In the above example, we have two dates, `date_1` and `date_2`, and we use the `:calendar.compare` function to compare them. The function returns the atom `:lt` which stands for "less than". This indicates that `date_1` comes before `date_2`.

We can also use the `:calendar.compare` function to check if two dates are equal.

```elixir
# Comparing two equal dates using the `:calendar.compare` function
date_1 = ~D[2020-12-25]
date_2 = ~D[2020-12-25]

:calendar.compare(date_1, date_2) # returns :eq (equal)
```

As expected, the function returns the atom `:eq` indicating that the dates are equal.

If we want to check if one date comes after another, we can use the `:calendar.compare` function along with the `:gt` (greater than) atom.

```elixir
# Comparing two dates where one is greater than the other
date_1 = ~D[2021-03-01] # March 1st, 2021
date_2 = ~D[2021-02-15] # February 15th, 2021

:calendar.compare(date_1, date_2) # returns :gt (greater than)
```

In the above example, the function returns the atom `:gt`, indicating that `date_1` comes after `date_2`.

## Deep Dive

Behind the scenes, the `:calendar.compare` function uses the `:erlang.compare` function. This function takes two arguments and returns `-1` (if the first argument is less than the second), `0` (if the arguments are equal), or `1` (if the first argument is greater than the second).

Elixir converts the atoms returned by `:erlang.compare` into more meaningful atoms like `:lt`, `:eq`, and `:gt`. This makes the code more readable and easier to understand.

## See Also

- [Elixir Documentation on Date and Time](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir Documentation on Comparisons](https://hexdocs.pm/elixir/Kernel.html#==/2)