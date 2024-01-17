---
title:                "Comparing two dates"
html_title:           "Bash recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is a common task in programming, where we compare two sets of date and time values to determine their relationship. This is often done to check if one date is before, after, or equal to another date. Programmers do this to validate user input, sort data, and control program flow based on specific dates.

## How to:
To compare two dates in Bash, we can use the built-in ```test``` command, also known as ```[ ]```, along with the operators ```-gt```, ```-lt```, and ```-eq``` to compare greater than, less than, and equal to respectively. We can also use the ```date``` command to convert the dates into a format that can be compared. Below are some examples:

- To check if 10th April 2021 is after 1st January 2021:
```Bash
[ 20210410 -gt 20210101 ] && echo "10th April 2021 is after 1st January 2021"
```
Output: ```10th April 2021 is after 1st January 2021```

- To check if the current date and time is before 31st December 2021, we can use the ```date``` command:
```Bash
[ $(date +%Y%m%d%H%M%S) -lt 20211231000000 ] && echo "Current date and time is before 31st December 2021"
```
Output: ```Current date and time is before 31st December 2021```

- We can also compare dates in alternate formats, such as with ```date -d``` command, which takes a string as an argument and converts it into a date:
```Bash
[ $(date -d "2021-06-25" +%s) -gt $(date -d "2021-06-24" +%s) ] && echo "25th June 2021 is after 24th June 2021"
```
Output: ```25th June 2021 is after 24th June 2021```

## Deep Dive:
Comparing dates is not a new concept, as dates have been used to keep track of time for centuries. In Bash, the ```test``` command is used to check for certain conditions and return an exit status based on the outcome. This exit status is then used to control the flow of the program. As an alternative, we can also use the ```((${var#pattern}))``` construct, which checks for a specific pattern within a variable and returns a boolean result.

Furthermore, the implementation of comparing dates in Bash depends on the specific scenario and the desired output. Dates can be compared using various formats, such as epoch (seconds since 1st January 1970), day of the week, or day of year. It is essential to carefully choose the date format to ensure accurate comparisons.

## See Also:
- Bash ```test``` command: https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html
- Bash Arithmetic Expansion: https://www.gnu.org/software/bash/manual/html_node/Arithmetic-Expansion.html
- Date command in Bash: https://www.gnu.org/software/bash/manual/html_node/Bash-as-an-integer_002dcomparison-operator.html
- Date formats in Bash: https://www.gnu.org/software/bash/manual/html_node/Date-Methods.html#Date-Methods