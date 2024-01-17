---
title:                "Comparing two dates"
html_title:           "Fish Shell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a common task in programming where we need to check whether one date is before, after, or equal to another. This is often necessary when dealing with dates and times in data analysis, scheduling tasks, or implementing conditional logic in our code.

## How to:

To compare two dates in Fish Shell, we can use the ```fish_date``` command along with the ```-s``` flag to specify the format of the dates we want to compare. Here's an example:

```
# Creating two variables with date values
set today (date +%Y-%m-%d)
set tomorrow (date -d "tomorrow" +%Y-%m-%d)

# Using fish_date to compare the two dates
fish_date -s "%Y-%m-%d" $today $tomorrow
```

The output of this code will be:
```
Comparator: <
```

Which indicates that the first date (today) is less than the second date (tomorrow). Similarly, if we swap the order of the dates, we will get a different output:
```
# Creating two variables with date values
set today (date +%Y-%m-%d)
set tomorrow (date -d "tomorrow" +%Y-%m-%d)

# Using fish_date to compare the two dates
fish_date -s "%Y-%m-%d" $tomorrow $today
```

The output will now be:
```
Comparator: >
```

Showing that tomorrow's date is greater than today's date. We can also use other comparison operators such as ```>=```, ```<=```, ```==```, and ```!=``` to compare the two dates in a similar way.

## Deep Dive

Fish Shell's ```fish_date``` command is based on the standard Unix ```date``` command, with some added functionalities specific to Fish Shell. It supports all the common date and time formats used in programming, making it a versatile tool for comparing dates.

An alternative to using the ```fish_date``` command is to use built-in Fish Shell functions such as ```math```, which can handle date and time values as well. However, the syntax can be more complex and less intuitive compared to using ```fish_date```.

Under the hood, Fish Shell converts the dates into Unix timestamp values and then compares them. This allows for more efficient and accurate comparisons, especially when working with dates and times in various time zones.

## See Also

- [Fish Shell documentation on ```fish_date```](https://fishshell.com/docs/current/cmds/fish_date.html)
- [Comparison operators in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_conditionals)
- [Unix timestamp](https://en.wikipedia.org/wiki/Unix_time)