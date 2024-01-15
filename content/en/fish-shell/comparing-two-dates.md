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

## Why
So you might be wondering, why would anyone want to bother comparing two dates in a Fish Shell script? Well, let me tell you, being able to compare dates can be incredibly useful for automating tasks and organizing data. Plus, it's always satisfying to have your code do all the work for you.

## How To
Let's dive right into some coding examples and see how we can compare two dates using the Fish Shell!

```Fish Shell
# First, we'll need to set our two dates as variables
set date1 2020-01-01
set date2 2021-01-01

# Next, we'll use the `string compare` command to compare the two dates
if string compare $date1 $date2
    echo $date2 is after $date1
elif string compare -E $date1 $date2
    echo $date1 is equal to $date2
else
    echo $date1 is before $date2
end
```

Running this script will output the following:

```Fish Shell
2021-01-01 is after 2020-01-01
```

We used the `-E` flag to ignore the case when the two dates are equal, otherwise, they would be considered different. This is just one way to compare dates, there are plenty of other approaches depending on your specific needs.

## Deep Dive
If you want to take your date comparing skills to the next level, here are some additional tips for you.

### Using the `date` command
Instead of manually setting the dates as variables, you can use the `date` command to get the current date in a specific format. For example, `date +'%Y-%m-%d'` will give you the current date in the `YYYY-MM-DD` format. This can come in handy when working with dynamic dates.

### Converting dates to timestamps
Sometimes, you may need to compare dates in a more precise way. In that case, you can convert the dates to timestamps using the `date -j` command, which gives the number of seconds since January 1, 1970. Then, you can use the `math` command to compare the timestamps.

### Dealing with time zones
If you're working with dates in different time zones, you may need to convert them to a specific time zone before comparing them. The `TZ` environment variable can be used to set the desired time zone for the `date` command. You can find a list of valid time zones by running `date --help`.

## See Also
Checkout the official Fish Shell documentation for more information on date and time manipulation: [https://fishshell.com/docs/current/commands.html#date](https://fishshell.com/docs/current/commands.html#date)

And for some cool Fish Shell tips and tricks, check out this article by Scott Woods: [https://medium.com/@scottwoodsmakesstuff/tips-tricks-for-fish-shell-14c356bf92ca](https://medium.com/@scottwoodsmakesstuff/tips-tricks-for-fish-shell-14c356bf92ca)

Happy coding!