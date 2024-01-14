---
title:                "Bash recipe: Getting the current date"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the current date in your Bash script? Maybe you want to include it in a file name, or perhaps you need to keep track of when a certain task was executed. Whatever the reason may be, getting the current date in Bash is a useful skill to have. In this blog post, we will explore how to get the current date using simple Bash commands.

## How To

To get the current date in Bash, we can use the `date` command. This command is used to print or manipulate the current date and time. Let's take a look at some examples using this command.

```Bash
# To simply print the current date
date
# Output: Wed May 5 15:40:23 UTC 2021

# To get only the current year
date +"%Y"
# Output: 2021

# To get only the current month
date +"%m"
# Output: 05

# To get only the current day
date +"%d"
# Output: 05

# To get the current date in a specific format
date +"%A, %B %d %Y"
# Output: Wednesday, May 05 2021

# To store the current date in a variable
today=$(date +"%m-%d-%Y")
echo "Today's date is $today"
# Output: Today's date is 05-05-2021
```

## Deep Dive

The `date` command has many options that allow us to customize the output according to our needs. You can view the full list of options by typing `man date` in your terminal. Here are some common options that you might find useful:

- `%Y` : current year
- `%m` : current month
- `%d` : current day
- `%A` : full day of the week
- `%B` : full month name
- `%H` : current hour (24-hour format)
- `%M` : current minute
- `%S` : current second

Additionally, you can use the `date` command with the `-d` option to get the current date at a specific time. For example, to get the date and time 5 days from now, you can use `date -d "+5 days"`. This can be useful for scheduling tasks in your Bash script.

## See Also

- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Linuxize: How to Format Date and Time in Linux Using Date Command](https://linuxize.com/post/how-to-format-date-and-time-in-linux-using-date-command/)
- [TecMint: How to Use ‘date’ Command in Linux for Time Reference](https://www.tecmint.com/linux-date-command-for-time-reference/)