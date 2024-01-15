---
title:                "Converting a date into a string"
html_title:           "Bash recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in Bash programming, especially when dealing with data logging or generating file names. It allows us to manipulate and format dates according to our needs, making our scripts more flexible and efficient.

## How To

To convert a date into a string, we can use the `date` command followed by the format specifier we want to use. For example:

```Bash
date +"%Y-%m-%d"
```

This code will output the current date in the format of "year-month-day", such as 2021-09-01. We can also add additional specifiers to include the time or timezone.

```Bash
date +"%Y-%m-%d %H:%M:%S %Z"
```

This will output the date and time in the format of "year-month-day hour:minute:second timezone", such as 2021-09-01 09:30:00 EST.

To convert a specific date into a string, we can use the `-d` flag followed by the date we want to convert.

```Bash
date -d "September 1, 2021" +"%Y-%m-%d"
```

This will output the date in the specified format. We can also use the `-r` flag to convert a timestamp into a string.

```Bash
date -r 1630491000 +"%Y-%m-%d"
```

This will convert the timestamp 1630491000 (equivalent to September 1, 2021) into a string.

## Deep Dive

The `date` command uses the system's time and date information to generate the output. However, we can also use the `strftime` function in our Bash scripts to manipulate and convert a date into a string.

The `strftime` function takes in two arguments: the format specifier and the date or timestamp. It then returns the formatted string representation of the date or timestamp.

```Bash
strftime("%Y-%m-%d", 1630491000)
```

This will also output the date in the format of "year-month-day". Additionally, the `date` command supports more specifiers compared to the `strftime` function. We can see a list of all the format specifiers by running `man date` in the terminal.

## See Also

- [Bash Documentation on date command](https://www.gnu.org/software/bash/manual/html_node/Bash-Date-Input-Formats.html#Bash-Date-Input-Formats)
- [Bash strftime function documentation](https://www.gnu.org/software/bash/manual/html_node/Bash_002disms.html#index-strftime)