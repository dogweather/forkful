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

Fetching the current date refers to obtaining the exact day, month and year at a particular moment in time. This is crucial in programming for logging activities, timestamping data entries, and implementing features based on real-time or date-specific conditions.

## How to:

Getting the current date in Elm is quite straightforward. Here's a simple snippet:
```Elm
import Time

currentDate : Task.Task Time.Error Time.Posix
currentDate =
    Time.now
```
Within Elm, the `Time.now` function is not synchronous like in some other languages. It creates a `Task` instead; when executed, the task will give us the current time.

## Deep Dive

Historically, obtaining the current date and time was a simple task in many languages, but often led to common mistakes related to time zones and daylight saving time. Elm, eager to avoid such pitfalls, provides the `Posix` time representation that captures an instant in time, independent of time zones.

Whilst `Time.now` is probably the most used technique, remember that it returns a `Task`. This behavior can be surprising if coming from a language like JavaScript, where `Date.now()` returns the current date as a simple value. Tasks in Elm enable handling asynchronous operations and side-effects in a pure functional language.

The specific time format you use (ISO, Unix, etc.) will mostly depend on your particular needs. Elm provides `Time.toIsoString`, `Time.fromIsoString`, `Time.toMillis` and `Time.fromMillis` functions to convert `Posix` values to and from these common formats.

## See Also

Elm's documentation provides more specific details about working with time:

1. [Elm - Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
2. [Elm - Date and Time](https://elmprogramming.com/date-and-time.html)
3. [Elm - Tasks](https://guide.elm-lang.org/effects/tasks.html) (For understanding how `Time.now` works)