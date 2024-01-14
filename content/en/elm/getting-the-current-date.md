---
title:    "Elm recipe: Getting the current date"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

As programmers, we are often faced with the task of displaying the current date in our applications. This could be for a variety of reasons such as displaying the date on a website, generating a timestamp for a user action, or simply for logging purposes. Whatever the reason may be, it is a fundamental task that we need to know how to accomplish. In this blog post, we will explore how to get the current date in Elm and discuss why it is important.

## How To

To get the current date in Elm, we can use the `Date.now` function from the `elm/time` package. The `Date.now` function returns a `Time` value representing the current time in milliseconds. We can then use this value to format the date in any way that we need.

Let's take a look at an example of how we can get the current date in Elm and format it as a string:

```
import Html exposing (text)
import Time exposing (..)

currentTime : Time
currentTime = Date.now 

-- Format the date as a string
formattedDate : Time -> String
formattedDate time =
    time
        |> utcToZoned Time.zone
        |> Time.format "%b %d, %Y"

main =
    text (formattedDate currentTime)
```

This code uses the `utcToZoned` function to convert the `Time` value into our local timezone and then uses the `Time.format` function to format it as a string in the format we desire. In this case, we have used the "%b %d, %Y" format, which will give us a result like "Sep 10, 2021". 

We can also use other formatting options to get the date in different formats. For example, we can use "%Y-%m-%d" to get the date in the format "2021-09-10". You can explore the different formatting options in the `Time` module documentation.

## Deep Dive

Getting the current date may seem like a simple task in Elm, but there are some important things to keep in mind. First, the `Date.now` function returns a `Time` value, which is a type alias for an integer representing the number of milliseconds since the Unix epoch (January 1, 1970). This means that the `Date.now` function is not specific to Elm and is a standard JavaScript function.

Another thing to keep in mind is that the `Date.now` function will return the current time in UTC. This means that if you want to display the date in your local timezone, you will need to convert it using the `utcToZoned` function as shown in the previous example.

Lastly, it is worth noting that the `Date.now` function is not affected by changes in the system clock. This means that if a user changes the date or time on their device, it will not affect the result of the `Date.now` function. 

## See Also
- Elm documentation for the `elm/time` package: https://package.elm-lang.org/packages/elm/time/latest
- MDN Web Docs for the `Date.now` function in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
- Moment.js library for formatting dates in JavaScript: https://momentjs.com/