---
title:                "Elm recipe: Converting a date into a string"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Why
If you're familiar with Elm, you may have come across the need to convert a date into a string. This is a common task when working with user input or displaying dates in a readable format. In this blog post, we'll explore why converting dates to strings is useful and how it can be done in Elm.

##How To
Converting a date into a string in Elm is a straightforward process and can be done using the `format` function from the `Time` library. Let's look at an example:

```Elm
import Time

dateToConvert = Time.posixToMillis 1549560000 --Monday, February 7th, 2019 at 14:00:00

formattedDate = Time.format "%A, %B %d, %Y at %H:%M:%S" dateToConvert

```

In this example, we first import the `Time` library, which provides functions for working with dates and times. Then, we create a variable `dateToConvert` which represents a specific date in milliseconds since January 1st, 1970. This is a common format used for dates in programming.

Next, we use the `format` function to convert our date into a string using the specified format string. This function takes two arguments - the first being the format string, and the second being the date to format.

In our example, we use the format string `"%A, %B %d, %Y at %H:%M:%S"` which represents the format we want our date to be displayed in. The letters A, B, d, Y, H, M, and S are placeholders that will be replaced with the actual values from our date. You can find a list of these placeholders and their meanings in the Elm documentation for `Time`.

The `format` function then returns a string representation of our date, which we store in the `formattedDate` variable. Printing `formattedDate` to the console would result in the string "Monday, February 7, 2019 at 14:00:00".

##Deep Dive
Now that we have a basic understanding of how to convert a date into a string in Elm, let's dive deeper and explore some other useful functions and formatting options.

1. `posixToTime`: This function converts a date in milliseconds since January 1st, 1970 into a `Time.Posix` value, which can then be used with `format` to convert into a string. This is useful when working with user input or dates from an external API.

2. `Time.Year`, `Time.Month`, `Time.Day`, etc.: These functions can be used to extract specific parts of a date, such as the year, month, or day, and can be combined with the placeholders in the format string to customize the output.

3. `Time.inUtc`: This function converts a `Time` value to Coordinated Universal Time (UTC), which is helpful when working with dates in different timezones.

There are also other formatting options available for customizing the output, such as adding leading zeros to numbers or displaying the time in a 12-hour format. You can find more information about these options in the `Time` documentation.

##See Also
- [The `Time` package documentation](https://package.elm-lang.org/packages/elm/core/latest/Time)
- [Elm Guide: Dates and Times](https://guide.elm-lang.org/dates_and_times/)
- [Elm Time Cheatsheet](https://github.com/TimelessLearner/elm-time-cheatsheet)