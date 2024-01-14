---
title:                "Bash recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Bash is a popular open-source command line interface used for automating tasks and managing systems. One common task in Bash programming is converting a date into a string, which can be useful for creating filenames, timestamps, or formatting data. In this blog post, we will explore the reasons why someone would engage in this task and learn how to do it effectively.

## How To

Converting a date into a string in Bash is a straightforward process. To begin, we need to use the `date` command, which is used to display and manipulate date and time information. The syntax for using `date` is as follows:

```Bash
date +"FORMAT"
```

Here, `FORMAT` specifies the output format of the date in string form. For example, if we want to convert the current date into a string in the format of "MM-DD-YYYY", we would use the following command:

```Bash
date +"%m-%d-%Y"
```

Running this command in the terminal will output the current date in the desired format, such as "08-04-2021". We can also specify a specific date by using the `-d` option, followed by the desired date in quotes. For example, to convert the date "January 1st, 2021" into a string in the format of "YYYY-MM-DD", we would use:

```Bash
date +"%Y-%m-%d" -d "January 1st, 2021"
```

This would output "2021-01-01". The `date` command also allows us to customize the output further by including other time elements such as hours, minutes, and seconds. For a full list of available options for the `date` command, you can refer to the [GNU Coreutils manual](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html).

## Deep Dive

Behind the scenes, the `date` command uses the `strftime()` function, which is a common function used in programming languages to format dates and times. The `FORMAT` argument used in the `date` command follows the syntax of `strftime()`, with some minor variations. For example, the `%Y` in `date +"%Y-%m-%d"` corresponds to the full year, whereas in `strftime()`, it is represented as `%Yy`. Understanding this connection can help us use the `date` command more efficiently.

Additionally, the `date` command also allows us to manipulate dates by adding or subtracting time to a given date using the `-d` option and a simple math expression. For example, to get the date one week from the current date, we can use the following command:

```Bash
date +"%m-%d-%Y" -d "+1 week"
```

This would output a date that is one week ahead of the current date. This feature can be particularly useful when creating scripts or automating tasks that require manipulating dates and times.

## See Also

- [GNU Coreutils manual on date input formats](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)
- [Bash scripting tutorial for beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Using the date command in Linux](https://www.howtoforge.com/linux-date-command/)

By mastering the conversion of dates into strings in Bash, you can add powerful functionality to your scripts and enhance your system management skills. I hope this blog post has provided you with a clear understanding of how to do this task effectively. Happy coding!