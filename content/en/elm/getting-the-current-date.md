---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date means retrieving the current date and time from the system. Programmers often do this to display the current date and time on their applications or keep track of when certain events occurred.

## How to:
```
Elm.Time.now
```

This function from the Elm Time module returns the current date and time as a Timestamp value. It can be called on its own or combined with other functions to manipulate the current date and time.

Example:
```
Elm.Time.now -- returns the current date and time
|> Elm.Time.toDate -- converts the Timestamp value to a Date value
|> Elm.Date.toIsoString -- converts the Date value to an ISO string
```
Output: "2021-09-21T10:00:00.000Z" (output may vary depending on the current date and time)

## Deep Dive:
### Historical Context:
The concept of getting the current date and time has been around since the early days of computing. Originally, programmers had to manually enter the current date and time into their programs. With the development of operating systems, accessing the system's current date and time became possible.

### Alternatives:
Elm has a few alternative functions for retrieving the current date and time, such as `Elm.Time.millisToUtc`, `Elm.Date.toUtc`, and `Elm.Date.toIsoString`. However, `Elm.Time.now` is the simplest and most widely used method.

### Implementation Details:
When you call `Elm.Time.now`, it makes a request to the system's clock and returns the current time as a Timestamp value. The Timestamp value is a record with fields for year, month, day, hour, minute, second, and millisecond. In order to get a more user-friendly date and time format, you can use additional functions to convert the Timestamp value to a Date value and then to a string.

## See Also:
- Elm Time module documentation: https://package.elm-lang.org/packages/elm/time/latest/
- MDN Date object: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date