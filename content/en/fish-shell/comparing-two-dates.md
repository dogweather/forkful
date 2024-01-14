---
title:                "Fish Shell recipe: Comparing two dates"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Why

When working with dates in programming, it can be useful to compare them in order to determine which one is earlier, later, or if they are equal. This can be particularly helpful in sorting data or validating user input. In this blog post, we will explore how to compare two dates using the Fish Shell programming language.

##How To

Comparing dates in Fish Shell is a simple process. You can use the `date` command to display the current date, and the `-d` option to specify a different date to compare. For example:

```Fish Shell
date -d "2021-01-01"
```

This will display the date in the specified format. Now let's compare two dates and see the output:

```Fish Shell
if [ "2021-01-01" > "2020-12-31" ]
  echo "2021-01-01 is later than 2020-12-31"
else
  echo "2021-01-01 is not later than 2020-12-31"
end
```

The output will be:

`2021-01-01 is later than 2020-12-31`

As you can see, the `>` symbol is used to indicate that the first date is later than the second one. Similarly, you can use `<` and `=` to check for earlier or equal dates.

Another useful command for comparing dates is `date -s`, which allows you to specify a date in the shell. This can be helpful when comparing user input or dates from a file. Here's an example:

```Fish Shell
set mydate "2021-01-01"
date -s $mydate
if [ (date +%Y-%m-%d) > $mydate ]
  echo "New date is set to a later date"
end
```

This script will set the date to January 1st, 2021 and then check if the current date is later than the one we entered. If it is, the output will be:

`New date is set to a later date`

##Deep Dive

When comparing dates, it's important to keep in mind that the format of the date can affect the comparison. For example, using the format `DD-MM-YYYY` will give different results compared to `YYYY-MM-DD`. Make sure to use a consistent format for accurate comparisons.

Additionally, when using the `>` or `<` symbols, the comparison is done based on the string value of the dates. This means that dates with a higher number will be deemed later, even if they are technically earlier. For example, `2021-01-01` will be considered later than `2020-12-31` because `2` is higher than `1`.

Finally, you can also use the `-d` option with the `date` command to specify a specific time along with the date. This can be useful for more precise comparisons, such as determining if one date is within a certain time range of another date.

##See Also

For more information on working with dates and times in Fish Shell, check out these links:

- [Fish Shell documentation on `date` command](https://fishshell.com/docs/current/cmds/date.html)
- [Stack Overflow thread on comparing dates in Fish Shell](https://stackoverflow.com/questions/55632433/how-do-i-compare-dates-in-the-fish-shell)