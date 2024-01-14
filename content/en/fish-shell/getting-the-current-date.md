---
title:    "Fish Shell recipe: Getting the current date"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

There are many reasons why someone might want to get the current date in their Fish Shell programming. One common use case is to include the date in file names or log entries for organization and tracking purposes.

## How To

To get the current date in Fish Shell, use the `date` command followed by the desired format. Here are a few examples:

```
Fish Shell:

# Date in YYYY-MM-DD format
date "+%F"

# Date in Month DD, YYYY format
date "+%B %d, %Y"

# Date in Day of Week, Month DD, YYYY HH:MM format
date "+%A, %B %d, %Y %H:%M"
```

Sample output for the above commands would be:

```
2019-09-09
September 09, 2019
Monday, September 09, 2019 09:45
```

In the `date` command, the `+` sign indicates that we are specifying a custom output format. The following characters represent different date and time elements, such as `%Y` for the year in four digits and `%H` for the hour in 24-hour format. You can find a full list of these characters in the Fish Shell documentation.

## Deep Dive

Behind the scenes, Fish Shell uses the `strftime` function from the standard C library to format the date and time. This function takes a string as input and replaces certain characters with their corresponding date and time elements. It also provides additional options for things like local time and time zone.

If you want to get the current date and time in a different time zone, you can use the `TZ` environment variable. For example, to get the date and time in Eastern Standard Time, you can do:

```
Fish Shell:

# Date and time in Eastern Standard Time
TZ=EST date "+%F %H:%M"
```

This would output something like `2019-09-09 04:45`, indicating that in EST, it is currently 4:45 AM. This is a useful tool for remote team collaboration or for tracking events happening in different time zones.

## See Also

- [Fish Shell date documentation](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell strftime man page](https://man.archlinux.org/man/strftime.3.en)