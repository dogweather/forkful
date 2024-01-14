---
title:    "Bash recipe: Getting the current date"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever wondered what the current date is? Whether it's for a project or just out of curiosity, being able to access the current date through Bash programming can be extremely useful. In this blog post, we will explore how to get the current date using Bash and why it is important.

## How To

Getting the current date in Bash is simple and can be done using the `date` command. Let's take a look at an example:

```
Bash
$ date
Mon Jan 18 21:34:12 EST 2021
```

We can see that the output includes the day of the week, month, day, time, and timezone. But what if we only want to see the date in a specific format? We can use the formatting option `-d` to specify the format.

```
Bash
$ date -d "today"
Mon Jan 18 00:00:00 EST 2021

$ date -d "today" +"%m-%d-%Y"
01-18-2021
```

The first command will display the current date at midnight, while the second command will display the date in the format of month-day-year. You can use various formatting options to display the date in any way you want.

But what if we want to get the date for a specific day? We can use the `-d` option again, but this time we will specify the day.

```
Bash
$ date -d "next Friday"
Fri Jan 22 00:00:00 EST 2021
```

This will display the date for the upcoming Friday.

## Deep Dive

The `date` command uses the system clock to determine the current date and time. You can also use the `date` command to set or change the system clock. To do so, you will need to use the `-s` option followed by the desired date and time in the format of MMDDhhmmYYYY.ss.

```
Bash
$ date -s "011821002021.00"
Mon Jan 18 00:20:00 EST 2021
```

This will set the system clock to January 18, 2021 at 12:20 AM.

The `date` command also has other useful options such as `-u` to display the UTC time, `-I` to display the date in ISO format, and `-R` to display the date in RFC822 format.

## See Also

- [Linuxize - How To Get Current Date and Time on Linux](https://linuxize.com/post/how-to-get-current-date-and-time-in-bash/)
- [SS64 - Linux: date](https://ss64.com/bash/date.html)
- [Linux Handbook - How to Use the `date` Command in Linux](https://linuxhandbook.com/date-command/)