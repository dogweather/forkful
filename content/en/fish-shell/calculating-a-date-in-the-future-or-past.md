---
title:                "Calculating a date in the future or past"
html_title:           "Fish Shell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is the act of determining a date that is a certain number of time units away from a given date. This can be useful for scheduling tasks or events, setting reminders, or simply keeping track of time. Programmers often need to perform this task in their code to automate processes or make their programs more user-friendly.

## How to:

Fish Shell provides a built-in function called ```date``` that makes it easy to calculate a date in the future or past. Simply enter the following command in your Fish Shell terminal:

```
date +%F -d "YYYY-MM-DD + X days/weeks/months"
```

Replace ```YYYY-MM-DD``` with the starting date and ```X days/weeks/months``` with the desired time difference. The output will be the new calculated date in the format of ```YYYY-MM-DD```.


To calculate a date in the past, use the following command:

```
date +%F -d "YYYY-MM-DD - X days/weeks/months"
```

For example, if you want to know the date 3 weeks from now, the command would be:

```
date +%F -d "2021-08-20 + 3 weeks"
```

And the output would be:

```
2021-09-10
```

## Deep Dive:

This technique of calculating a date in the future or past has been around since the early days of programming. Before built-in functions like ```date```, programmers would have to write complex algorithms to perform this task. However, with the advancements in technology and programming languages, this task has become much simpler.

There are also alternative methods for calculating a date in the future or past, such as using third-party libraries or using other built-in functions in programming languages like Python. However, using the built-in ```date``` function in Fish Shell is quick and convenient.

The implementation of the ```date``` function in Fish Shell uses a combination of system calls and low-level date manipulation functions from the C programming language. This is why the syntax for the command may seem unfamiliar to some programmers.

## See Also:

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html) for more information on the ```date``` function and other built-in functions.
- [Python tutorial](https://www.programiz.com/python-programming/datetime) on calculating dates in Python using the datetime library.
- [C library documentation](https://www.gnu.org/software/libc/manual/html_node/Time-Types.html) for a deeper understanding of date manipulation functions.