---
title:                "Getting the current date"
html_title:           "Fish Shell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to quickly check the current date without having to open your calendar app or search online? With Fish Shell, you can easily get the current date right in your terminal, saving you time and hassle.

## How To

First, open your terminal and enter the following command:

```
date
```

This will output the current date and time in the format specified by your system settings. 

To customize the format, you can use the "-f" flag followed by a formatting string. For example, if you want the date to be displayed in the format of "Month/Day/Year", you can use the following command:

```
date -f "%m/%d/%Y"
```

This will output the date in the desired format. You can use any combination of formatting characters to customize the output according to your preference.

## Deep Dive

Behind the scenes, the "date" command in Fish Shell is utilizing the "date" function from the GNU Core Utilities. This function takes into account your system's current date and time and formats it accordingly. 

It also has the ability to perform various operations such as adding or subtracting time by using flexible input options. For a comprehensive list of available options, you can consult the official GNU Core Utilities documentation. 

In addition, Fish Shell also has an internal function called "strftime" which allows for additional customization of the date output. This function implements the same formatting options as the "date" function, but also includes some additional characters for more advanced formatting. 

## See Also

- [GNU Core Utilities Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Fish Shell Documentation on strftime function](https://fishshell.com/docs/3.1/cmds/strftime.html)