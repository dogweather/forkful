---
title:                "Fish Shell recipe: Getting the current date"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
In today's fast-paced world, keeping track of the current date and time is crucial for staying organized and efficient. Whether it's for work, school, or personal responsibilities, having accurate dates is essential. That's where the Fish Shell's built-in capabilities come in handy. With just a simple command, you can easily obtain the current date and time.

## How To
Coding in Fish Shell is incredibly user-friendly and can be done even by beginners. To get the current date, simply use the `date` command followed by the desired output format. For example, if you want to display the current date in the "Month Day, Year" format, you would use the following code:

```
Fish Shell
date "+%B %e, %Y"
```

This will output the current date in the format "June 25, 2021". You can also customize the format to your liking by referring to the Fish Shell documentation for the `date` command.

## Deep Dive
For those who want to dive a little deeper, let's explore some of the options available for the `date` command. For example, you can use the `-d` flag to specify a different date than the current one. This is useful when you need to get the date of a particular event or task. Additionally, you can use the `-R` flag to output the date in the RFC 2822 format, or the `-u` flag to display the date in UTC format.

You can also include time in your output by using the `%H` and `%M` placeholders for hours and minutes, respectively. For example, the code `date "+%B %e, %Y at %H:%M"` will display the current date and time in the format "June 25, 2021 at 11:30".

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix Tutorial: Using `date` Command](https://www.howtogeek.com/424288/how-to-use-the-date-command-on-linux/)
- [Fish Shell Tips and Tricks](https://github.com/jorgebucaran/fish-shell-cookbook)