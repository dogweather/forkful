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

## What & Why?

Getting the current date in Fish Shell is a way to retrieve the current date and time information. This is a common task for programmers as it allows them to timestamp events or automate tasks based on the current date.

## How to:

To get the current date in Fish Shell, you can use the `date` command. Here are two examples of how it can be used:

```
# Get the current date and time in the default format
Fish Shell> date
Sun Apr 25 12:00:00 EST 2021

# Get the current date in a custom format (YYYY-MM-DD)
Fish Shell> date +"%Y-%m-%d"
2021-04-25 
```

## Deep Dive

### Historical Context:

Getting the current date has been a common task for programmers since the early days of computing. As technology has advanced, so have the methods for retrieving this information.

### Alternatives:

Other alternatives for getting the current date in Fish Shell include using the `date` command with different options, or using third-party libraries for more advanced functionality.

### Implementation Details:

The `date` command in Fish Shell allows for various options to change the output format of the current date. These options follow the same syntax as the `date` command in the Unix operating system.

## See Also:

- [Fish Shell Documentation](https://fishshell.com/docs/current/cmds/date.html)
- [Unix Date command](https://www.geeksforgeeks.org/date-command-in-linux-with-examples/)