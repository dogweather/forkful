---
title:    "Bash recipe: Comparing two dates"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in your Bash script? Whether it's for scheduling tasks or validating user input, comparing dates is a common task in programming. In this blog post, we will explore how to compare two dates in Bash and discuss some important considerations.

## How To

To compare dates in Bash, we will use the `date` command along with the `-d` option to convert dates into seconds since epoch (January 1, 1970). This allows us to easily compare dates using simple arithmetic operations.

Let's take a look at a simple example. Say we have two variables, `date1` and `date2`, each containing a date in the format of `YYYYMMDD`.

```
#! /bin/bash

# Assign dates to variables
date1="20210625"
date2="20210703"

# Convert dates to seconds since epoch
date1_sec=$(date -d "$date1" +%s)
date2_sec=$(date -d "$date2" +%s)

# Compare dates using arithmetic operations
if (($date1_sec < $date2_sec));
then
   echo "$date1 is earlier than $date2."
elif (($date1_sec > $date2_sec));
then
   echo "$date1 is later than $date2."
else
   echo "Both dates are equal."
fi
```

Running this script will give us the output:

```
20210625 is earlier than 20210703.
```

In this example, we first use the `date` command with the `-d` option to convert our dates into seconds since epoch. Then, we use an `if` statement with arithmetic operations to compare the dates and print the appropriate message. We can easily change the dates in the variables and re-run the script to compare different dates.

## Deep Dive

When comparing dates in Bash, there are a few important things to keep in mind. Firstly, the format of the dates must be consistent. In our example, we used the format `YYYYMMDD`, but you can use any format as long as it is the same for both dates. Secondly, the `-d` option in the `date` command is very flexible, allowing you to use various date and time formats. Be sure to check the `man` page for more information.

Another important consideration is the use of leap years. Since our example converts dates to seconds since epoch, leap years will affect the comparison. For more accurate comparisons, you can also use the `+%j` option in the `date` command, which converts dates to day of year instead of seconds since epoch.

## See Also

For more information on the `date` command and its various options, check out the following links:
- `man` page for `date`: https://man7.org/linux/man-pages/man1/date.1.html
- Bash documentation on date formatting: https://www.gnu.org/software/bash/manual/html_node/Date-Manipulation.html
- Bash arithmetic expressions: https://www.gnu.org/software/bash/manual/html_node/Arithmetic-Expansion.html