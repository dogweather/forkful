---
title:                "Converting a date into a string"
html_title:           "Elm recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string simply means taking a date and turning it into a text format that is readable and understandable for humans. This is commonly done by programmers to display dates on a website or application in a way that is user-friendly and follows a specific format.

## How to:

To convert a date into a string in Elm, we make use of the `toString` function, which takes in a date as its parameter and returns a string representation of that date. Let's take a look at an example code below:

```
Elm.toString October 24 2021
```

The output of this code will be:

```
"Sun Oct 24 2021"
```

Alternatively, we can also specify a specific format for our output using the `Date.format` function. The code and output for this would look like:

```
Date.format "%A, %B %d %Y" October 24 2021
```

Output:

```
"Sunday, October 24 2021"
```

## Deep Dive

Prior to the invention of computers, people used to write dates in a long-form textual format, such as "Sunday, October 24, 2021." However, with the increasing use of technology and the need for consistent formatting, dates started being represented in numeric formats, such as "10/24/2021" or "24.10.2021." However, this format is not easily readable for humans, hence the need for converting dates into strings.

In terms of alternatives, there are other functions in Elm that allow us to manipulate dates, such as `Date.add` to add or subtract time from a given date and `Date.fromTime` to convert a time value into a date. Additionally, there are also external libraries such as `elm-time` and `Time.Clock` that provide more robust date and time functionalities.

In terms of implementation, the `toString` and `Date.format` functions use the `Date` and `Time` modules in Elm to convert dates into strings. These modules also allow for more advanced manipulations of dates, such as comparing dates and checking for leap years.

## See Also

- [Elm official documentation for Date and Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Using Dates in Elm video tutorial](https://www.youtube.com/watch?v=-G8xTa6XMcA)
- [elm-time package on GitHub](https://github.com/elm/time)