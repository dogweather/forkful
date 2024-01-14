---
title:                "Bash recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often need to work with dates in our code. Whether it's displaying the current date on a website, organizing files by date, or creating a timestamp for data analysis, having the ability to get the current date using Bash programming can be extremely useful. It saves us from manually searching for the current date and typing it out, and it ensures that our code is always up to date with the current date.

## How To

To get the current date in Bash, we can use the `date` command. This command displays the current date and time in a specific format. For example, to display the date in the format of "Day of the week, Month Day, Year", we can use the following code:

```Bash
date +"%A, %B %d, %Y"
```

The output of this command would look something like this:

```Bash
Monday, September 20, 2021
```

We can also customize the format of the date according to our needs. For example, if we want to display the date in the format "Month/Day/Year", we can use the following code:

```Bash
date +"%m/%d/%Y"
```

The output would be:

```Bash
09/20/2021
```

We can also include the current time in our output by adding additional formatting options to the `date` command. For example, to display the current date and time in the format "Month/Day/Year Hour:Minute:Second", we can use:

```Bash
date +"%m/%d/%Y %H:%M:%S"
```

The output would be:

```Bash
09/20/2021 13:45:30
```

## Deep Dive

Behind the scenes, the `date` command is using the `C` programming language to get the current date from the system's clock. It then formats the date according to the provided options and displays it. We can also use the `date` command to get the current date in different time zones, by using the `-u` option for UTC time or by specifying the time zone using the `-I` option.

There are also various built-in variables in Bash that can be used to extract specific elements from the current date. For example, `$YEAR`, `$MONTH`, `$DAY`, etc. Using these variables, we can create more customized date and time outputs in our code.

## See Also

Here are some resources for further reading about getting the current date in Bash:

- [Bash Scripting Tutorial - Date and Time](https://ryanstutorials.net/bash-scripting-tutorial/bash-dates.php)
- [Bash Documentation - date command](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-date)