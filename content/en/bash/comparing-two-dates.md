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

## Why 

Comparing two dates is a common task in programming, especially when dealing with time-sensitive data or events. By learning how to compare dates in Bash, you can effectively manipulate and make decisions based on date values in your scripts or programs. 

## How To 

To compare two dates in Bash, you can use the `date` command combined with the `test` command or the `[[` syntax. Here's a sample code and its output: 

```Bash 
#!/bin/bash 

date1="2021-01-01" 
date2="2021-02-01" 

if [[ "$date1" > "$date2" ]]; then 
echo "Date 1 is later than Date 2" 
elif [[ "$date1" < "$date2" ]] 
echo "Date 2 is later than Date 1" 
else 
echo "The dates are equal" 
fi 
``` 
Output: 
`Date 2 is later than Date 1`

In the above code, we first define two variables `date1` and `date2` with values in the format `YYYY-MM-DD`. Then, we use the `[[` syntax and specify the comparison condition within double brackets. In this case, we're checking if `date1` is greater than `date2`. If this condition is true, the first statement will be executed. Otherwise, the `elif` condition will be checked, and if it's true, the second statement will be executed. If both conditions are false, the `else` statement will be executed. 

Alternatively, you can also use the `test` command with the `-gt` or `-lt` flags for greater than or less than comparisons, respectively, and the `-eq` flag for equal comparisons. Here's an example: 

```Bash 
#!/bin/bash 

date3="2021-03-01" 
date4="2021-04-01" 

if [ $date3 -gt $date4 ]; then 
echo "Date 3 is greater than Date 4" 
elif [ $date3 -lt $date4 ]; then 
echo "Date 4 is greater than Date 3" 
else 
echo "The dates are equal" 
fi 
``` 
Output: 
`Date 4 is greater than Date 3`

## Deep Dive 

When comparing dates in Bash, it's important to keep in mind that the format of the dates must be consistent. For example, if you want to compare dates in the `MM-DD-YYYY` format, make sure that all your dates follow the same format. 
In addition, the `date` command has various options for customizing the output of date values. For example, you can use the `-d` option to specify a date and time string instead of the current system date. This can come in handy when you need to compare a specific date and time combination. 

## See Also 

- [Bash Scripting Tutorial](https://linuxize.com/post/bash-scripting-tutorial/) 
- [Bash Tips: Comparing Numbers and Strings](https://www.tldp.org/LDP/abs/html/comparison-ops.html) 
- [GNU Coreutils date manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)