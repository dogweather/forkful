---
title:                "Comparing two dates"
html_title:           "Lua recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates in programming refers to the process of evaluating whether one date is earlier, later, or the same as another date. This is typically done using various programming functions and methods to retrieve and manipulate the date data. Programmers often compare dates when dealing with time-sensitive data, such as scheduling tasks or tracking events.

## How to:
To compare two dates in Lua, first, we need to obtain the date values using the `os.time()` function, which returns the number of seconds since January 1st, 1970 00:00:00 UTC. Then, we can use the comparison operators (`<`, `>`, `<=`, `>=`, `==`, `~=`) to compare the two date values. Here's an example of comparing the current date to a specified date:
```Lua
-- Obtaining current date
currentDate = os.time()

-- Obtaining specified date
specifiedDate = os.time{year=2020, month=12, day=25}

-- Comparing dates
if currentDate < specifiedDate then
  print("Christmas is coming up!")
elseif currentDate > specifiedDate then
  print("Christmas has passed.")
else
  print("It's Christmas today!")
end

-- Output: Christmas is coming up!
```

## Deep Dive:
Comparing dates has been a challenge for programmers since the beginning of programming languages. Different languages have their own unique methods of handling dates, and some struggle with exceptions such as leap years and time zones. In Lua, a popular alternative to using the `os.time()` function is the `date` library, which allows for more precise manipulation of date values. However, it requires additional formatting and conversion to perform date comparisons. For implementation details, check out the Lua documentation on date and os modules.

## See Also:
- Learn more about the `os.time()` function: https://www.lua.org/manual/5.3/manual.html#pdf-os.time
- Explore the `date` library in Lua: https://www.lua.org/pil/22.1.html
- Check out the Lua Standard Library Reference: https://www.lua.org/manual/5.3/manual.html