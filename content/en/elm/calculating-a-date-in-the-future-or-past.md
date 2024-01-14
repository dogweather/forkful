---
title:    "Elm recipe: Calculating a date in the future or past"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why

Have you ever needed to calculate a date in the future or past in your Elm program? Whether it's for scheduling events or managing project timelines, being able to accurately determine a specific date can be essential for many applications. In this blog post, we will discuss how to easily calculate a date in the future or past using Elm.

# How To

To calculate a date in the future or past, we will use the widely available Elm package "elm-time" which provides functions for working with dates and times. First, we need to import this package in our Elm program by adding the following line at the top of our file:

```Elm
import Time exposing (..)
```

Next, we can use the `add` function from the "elm-time" package to add a number of days, months, or years to a given date. For example, let's say we want to calculate a date 7 days from today, we can use the following code:

```Elm
add (days 7) (now)
```

The `now` function returns the current date and time. So, by adding 7 days to the current date, we will get a date 7 days in the future. Similarly, we can also subtract a certain number of days, months, or years from a date using the same `add` function.

In addition, the "elm-time" package provides many other functions for working with dates and times such as `toMonth`, `toYear`, `fromString`, etc. These functions can be very useful for manipulating dates and times in your Elm program.

To better understand how to use these functions, let's take a look at a code example below:

```Elm
import Time exposing (..)

-- Define a date
date = fromString "2021-04-01"

-- Calculate a date 2 years from the given date
futureDate = add (years 2) date

-- Display the year of the calculated date 
-- Returns 2023 as the output
toYear futureDate
```

# Deep Dive

If you want to dig deeper into calculating dates in the future or past, the "elm-time" package offers a great deal of documentation and tutorials on their website. You can also explore other available packages that provide additional functionalities for working with dates and times in Elm.

One important thing to keep in mind while working with dates and times in Elm is that the "elm-time" package uses a customized `Date` type to represent dates, rather than relying on the built-in `Time` type in Elm. This is because the "elm-time" package offers more precise and convenient functions for handling dates, making it easier to work with.

# See Also

- Official documentation for "elm-time" package: https://package.elm-lang.org/packages/elm/time/latest/
- Article on manipulating dates and times in Elm: https://medium.com/@ckoster22/manipulating-dates-and-times-in-elm-c8dab67f1e2a
- Additional packages for working with dates in Elm: https://package.elm-lang.org/packages/elm/time/latest/