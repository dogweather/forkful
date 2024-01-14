---
title:    "Bash recipe: Calculating a date in the future or past"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
We all have had a need to calculate a date in the future or past at some point in our programming journey. Whether it's for scheduling tasks or organizing events, being able to determine a specific date in relation to today's date can be extremely useful. In this blog post, we will explore how to write a Bash script that can calculate dates in the future or past and give a deeper understanding of the process.

## How To
To calculate a date in the future or past, we will be using the `date` command in Bash. This command allows us to display or manipulate the system's date and time. Here's an example of how we can use the `date` command to display today's date:

```
Bash
date +"%m/%d/%Y"
```

Running this command will output the date in the format of month/day/year. Now, let's say we want to calculate the date 5 days from now. We can use the `date` command again but this time, we will add 5 days to today's date by using the `+` symbol and the number 5 after it.

```
Bash
date -d "+5 days" +"%m/%d/%Y"
```

Running this command will output the date 5 days from today in the same format as before. We can also do the opposite and calculate a date that's 5 days ago by using the `-` symbol and the number 5.

```
Bash
date -d "-5 days" +"%m/%d/%Y"
```

We can also manipulate the date in other ways such as adding or subtracting weeks, months, or even years. For example, to calculate the date 2 weeks from today, we can use the `+` symbol followed by 2 weeks in this format: `+2 weeks`.

## Deep Dive
The `date` command in Bash uses the system's current time and date as its starting point. From there, we can use the `+` and `-` symbols to add or subtract time from that starting point. It's important to keep in mind that time and date calculations are based on Unix time. Unix time is a system for describing points in time, which is represented by the number of seconds that have elapsed since January 1, 1970, 00:00:00 UTC. This is known as the Unix epoch.

When using the `date` command, we can also specify a specific date to start from instead of using the current date and time. For example, we can use the `-d` flag followed by a specific date in this format: `MM/DD/YYYY`. This will then manipulate the specified date rather than the current date.

## See Also
- [Bash Scripting Tutorial for Beginners](https://linuxhint.com/bash_scripting_beginners_guide/)
- [Mastering Bash: An Introduction to Bash Scripting](https://www.codementor.io/awangga/mastering-bash-an-introduction-to-bash-scripting-14zl1wdzq3)
- [Unix Time](https://en.wikipedia.org/wiki/Unix_time)

By understanding how to calculate dates in the future or past using the `date` command in Bash, you will have an essential skill in your programming arsenal. With the ability to manipulate dates, you can streamline your Bash scripts and make tasks such as scheduling and organizing events much easier. Happy coding!