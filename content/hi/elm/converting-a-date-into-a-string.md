---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Elm: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Jab hum ek date ko string mein convert karte hai to hum us date ko readable format mein represent kar rahe hote hai. Programmers ki sabse common use case hai jab wo ek date ko website ya database mein store karna chahte hai. String format mein store karna unko flexibility deta hai aur dates ko alag-alag tarike se display karne ki aasani bhi deta hai.

# How to:

```Elm
import Time exposing (Date)
import Time.Format exposing (format)

dateToString : Date -> String
dateToString date =
    format "%Y-%m-%d" date
```

Yahan humne `Time` aur `Time.Format` libraries ko import kiya hai jiske andar `Date` aur `format` functions available hai. Phir humne `dateToString` function declare kiya hai jo ek `Date` type ka argument lega aur use `%Y-%m-%d` format mein string mein convert karega. Is format mein `%Y` saal, `%m` month aur `%d` day represent karte hai. Iske baad hum `dateToString` function ko sahi date ke sath call kar sakte hai aur uska output kuch iss tarah se hoga: `"2021-05-28"`

# Deep Dive

Converting dates into strings is not a new concept and has been used in programming for a long time. In fact, most programming languages provide built-in functions or libraries to make this task easier. Some alternatives to the `%Y-%m-%d` format used in our example are `%m/%d/%y`, `%d/%m/%y`, or even `%B %d, %Y` which displays the month name instead of a numeric representation.

Implementing this conversion can involve various steps such as parsing the date, checking for leap years, and formatting the output. While it may seem like a simple task, there are nuances and edge cases that programmers have to consider when working with dates and strings.

# See Also

1. Elm Time module: https://package.elm-lang.org/packages/elm/time/latest/
2. Time formatting in Elm: https://guide.elm-lang.org/effects/time.html#formatting-time