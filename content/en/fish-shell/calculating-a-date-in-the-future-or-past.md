---
title:    "Fish Shell recipe: Calculating a date in the future or past"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming projects. It allows you to manipulate and track dates, making it easier to manage deadlines, schedule events, and more. With Fish Shell, this process becomes even more efficient and simple.

## How To

Using Fish Shell, you can easily calculate dates in the future or past with just a few lines of code. Let's take a look at some examples:

```
# Calculate one week from today
set one_week (date -v+1w)
echo $one_week

# Calculate three months from a specific date
set start_date "2021/01/01"
set three_months (date -v+3m $start_date)
echo $three_months

# Calculate a specific date from today
set future_date (date -v+2021y)
echo $future_date
```

In the first example, we use the `date` command with the `-v` flag, which specifies a date to add or subtract from. In this case, we add one week from the current date. The output will be the date one week from today.

Similarly, in the second example, we start with a specific date and add three months to it. The output will be a date that falls three months from the start date. You can also use the `-v` flag with different units, such as years (`y`), months (`m`), weeks (`w`), and days (`d`).

Lastly, in the third example, we calculate a future date by specifying a specific year. This will output the date in the year 2021.

## Deep Dive

Behind the scenes, Fish Shell uses the `date` command to manipulate and calculate dates. The `-v` flag allows for easy manipulation of dates by adding or subtracting a specific amount of time. Additionally, you can use other flags and options with the `date` command to further customize your date calculations.

For more information about the `date` command and its different flags and options, you can check out the manual page by running `man date` in your terminal.

## See Also

- Fish Shell documentation: https://fishshell.com/docs/current/
- Using Arrays in Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_arrays
- Advanced usage of the `date` command: https://www.unix.com/man-page/osx/1/date/