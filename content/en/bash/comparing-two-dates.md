---
title:                "Bash recipe: Comparing two dates"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to compare two dates in your Bash programming but didn't know how? Comparing dates can be useful in many scenarios such as filtering files by date or checking for the latest version of a file. In this blog post, we will explore how to compare two dates in Bash programming.

## How To 

To compare two dates in Bash, we first need to understand how dates are represented in Bash. Dates in Bash can be stored in different formats, but the most common one is the UNIX timestamp, which represents the number of seconds that have elapsed since January 1st, 1970. For our example, we will use the `date +%s` command to get the current date in the UNIX timestamp format.

To compare two dates, we can use the `-gt` and `-lt` operators, which stand for "greater than" and "less than", respectively. For example, if we want to check if a date is greater than another date, we can use the following syntax:

```
if [ "$date1" -gt "$date2" ]; then
  echo "Date 1 is greater than Date 2"
fi
```

We can also use the `-eq` and `-ne` operators to check for equality and inequality, respectively. For a more extensive list of comparison operators in Bash, you can refer to this [documentation](https://tldp.org/LDP/abs/html/comparison-ops.html).

Let's look at an example. Suppose we want to check if the current date is greater than a specific date, which is one month ago. We can do this by getting the current date and the date one month ago in UNIX timestamp format and comparing them using the `-gt` operator. Here's the code:

```
current_date=$(date +%s)
one_month_ago=$(date -d "1 month ago" +%s)

if [ "$current_date" -gt "$one_month_ago" ]; then
  echo "Current date is greater than one month ago"
fi
```

The output of this code will be `Current date is greater than one month ago` if the current date is indeed greater than one month ago. This approach can also be used to check for other comparisons such as greater than or equal to, less than or equal to, etc.

## Deep Dive 

It is essential to note that when comparing dates in Bash, we need to make sure that they are converted to the same format. For example, if one date is in the UNIX timestamp format and the other is in the YYYY-MM-DD format, the comparisons might not work correctly. In such cases, we can use the `date -d` command to convert one of the dates to the desired format before comparing them.

We can also use other commands such as `date -u` or `date -r` to get the dates in different time zones or to convert them to different formats. The possibilities are endless, and it all depends on the specific use case.

## See Also 

- [Working with Dates in Bash](https://www.linuxjournal.com/content/working-dates-bash)
- [Bash Date Comparison](https://www.golinuxcloud.com/bash-date-comparison/)
- [Date Command in Bash](https://bash.cyberciti.biz/guide/The_date_command)