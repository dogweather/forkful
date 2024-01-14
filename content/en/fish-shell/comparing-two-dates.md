---
title:    "Fish Shell recipe: Comparing two dates"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

As programmers, we often come across the need to compare two dates in our code. This could be for various reasons, such as checking if one date is before or after another, finding the difference between two dates, or simply sorting a list of dates. In this blog post, we will explore how to compare two dates in Fish Shell and see how it can make our programming tasks easier.

## How To

First, let's start by creating two variables for our dates. We will use the `set` command to create variables and assign values to them. In this example, we will use the dates "2021-07-05" and "2021-07-10".

```
Fish Shell
set date1 2021-07-05
set date2 2021-07-10
```

Now, to compare these two dates, we will use the `-lt` and `-gt` operators. The `-lt` operator stands for "less than", and the `-gt` operator stands for "greater than". We will use these operators in an `if` statement to check if `date1` is before or after `date2`.

```
Fish Shell
if test $date1 -lt $date2
    echo "Date1 is before Date2"
else if test $date1 -gt $date2
    echo "Date1 is after Date2"
else
    echo "Date1 and Date2 are equal"
end
```

The output of this code will be "Date1 is before Date2", since "2021-07-05" comes before "2021-07-10". You can also use these operators in other contexts, such as sorting an array of dates.

## Deep Dive

Fish Shell has a built-in `date` command that allows for more complex date comparisons. This command uses the `%s` time format to represent dates in seconds, making it easier to compare them. For example, we can use the `date` command to find the difference between our two dates in seconds.

```
Fish Shell
date -f "%s" $date2; and not date -f "%s" $date1
```

This will output "432000", which is the number of seconds between "2021-07-05" and "2021-07-10". You can also use the `date` command to convert dates into different formats or perform other date/time operations.

## See Also

- Fish Shell documentation on dates: https://fishshell.com/docs/current/cmds/date.html
- Bash Scripting: Comparing Dates: https://www.shell-tips.com/bash/scripts/compare-dates-bash/