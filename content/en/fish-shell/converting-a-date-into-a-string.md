---
title:    "Fish Shell recipe: Converting a date into a string"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a date into a string using Fish Shell programming? Maybe you want to display the date in a specific format or use it as part of a file name. Regardless of the reason, understanding how to convert a date into a string can be a useful skill in your programming toolbox.

## How To

To convert a date into a string using Fish Shell, we can use the `date` command combined with the `fmt` command. Here's an example:

```
date +%Y-%m-%d | fmt
```

This command will display the current date in the format of "YYYY-MM-DD". The `date` command is used to retrieve the current date, while the `fmt` command formats the output into a string.

We can also specify a specific date to convert into a string, using the `-%s` option. Here's an example:

```
date -r 1588982400 +%B-%d-%Y | fmt
```

This command will display the date corresponding to the Unix timestamp 1588982400 in the format of "Month DD, YYYY". The `date` command is used with the `-r` option to specify a specific timestamp, and the `+%B-%d-%Y` option specifies the desired output format.

## Deep Dive

The `fmt` command has various options that allow for customization of the output string. For example, we can use the `-w` option to specify the desired width of the output. This can be useful when trying to align multiple date strings in a table.

Additionally, the `date` command also has multiple options for specifying the date or time format. For example, we can use the `%m` option to display the month in numerical form (01-12) instead of the default format of the month name.

Understanding these options and how to use them effectively can help us convert dates into strings that meet our specific needs.

## See Also

If you want to learn more about manipulating dates and times in Fish Shell, check out these resources:
- [Fish Shell documentation on the date command](https://fishshell.com/docs/3.1/cmds/date.html)
- [Fish Shell tutorial on manipulating dates and times](https://fishshell.com/docs/current/tutorial.html#tut_time)
- [Stack Overflow thread on converting dates into strings in Fish Shell](https://stackoverflow.com/questions/53590903/extract-date-to-string-in-bash-or-fish)