---
title:                "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

If you're an Elm programmer, you may have found yourself in a situation where you need to work with dates. Whether you're building a calendar application or need to display the current date on your website, it's important to know how to get the current date in Elm.

## How To

To get the current date in Elm, we can use the `Time` module. This module provides functions for working with dates, times, and time zones.

Let's start by importing the `Time` module:

```Elm
import Time
```

Next, we can use the `now` function to get the current date:

```Elm
now : Task x Time.Posix
```

As you can see, the `now` function returns a `Task` that will eventually resolve to a `Time.Posix` value, which represents the current date in the form of a Unix timestamp.

To get the current date in a more user-friendly format, we can use the `DateTime` module:

```Elm
import DateTime
```

Then, we can use the `fromPosix` function to convert the `Time.Posix` value to a `DateTime.DateTime` value:

```Elm
let
  currentDate = DateTime.fromPosix (Task.attempt identity now)
in
  currentDate
```

Running this code will give us the following output:

```Elm
2019 Jan 25, Friday 16:42:25
```

## Deep Dive

If you want to learn more about working with dates in Elm, you can explore the different functions and types in the `Time` and `DateTime` modules. You can also refer to the official documentation for more details and examples.

It's worth noting that the `Time` and `DateTime` modules only provide basic date and time functionality. If you need more advanced features, like date calculations or time zone conversions, you may need to use a third-party library.

## See Also

- [Time module documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [DateTime module documentation](https://package.elm-lang.org/packages/elm/time/latest/DateTime)

Now that you know how to get the current date in Elm, you can confidently incorporate dates into your Elm applications. Happy coding!