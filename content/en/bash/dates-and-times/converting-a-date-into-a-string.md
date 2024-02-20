---
date: 2024-01-20 17:35:51.086129-07:00
description: "Converting a date to a string means turning it into text that represents\
  \ the date. We do this to format dates for humans to read or to prepare them for\u2026"
lastmod: 2024-02-19 22:05:18.717591
model: gpt-4-1106-preview
summary: "Converting a date to a string means turning it into text that represents\
  \ the date. We do this to format dates for humans to read or to prepare them for\u2026"
title: Converting a date into a string
---

{{< edit_this_page >}}

## What & Why?
Converting a date to a string means turning it into text that represents the date. We do this to format dates for humans to read or to prepare them for storage in text formats like CSV or JSON.

## How to:
Below are examples of how to convert a date to a string in Bash:

```Bash
# Display the current date and time in the default format
echo $(date)

# Custom format: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# Include the time
echo $(date '+%Y-%m-%d %H:%M:%S')

# Convert an existing date
existing_date='2023-03-17 08:00:00'
date -d "$existing_date" '+%A, %B %d, %Y'
```
Sample output for the commands above:

```
Sat Mar 25 12:04:22 PDT 2023
2023-03-25
2023-03-25 12:04:22
Friday, March 17, 2023
```

## Deep Dive
Unix-like systems have used the `date` command from early on for handling date and time. Its flexibility allows for a myriad of formats, courtesy of format specifiers like `%Y` for year and `%d` for day.

There are alternatives to the `date` command if you're using a different tech stack. For instance, Python has `datetime.strftime`, while JavaScript offers the `Date` object with methods like `toLocaleDateString()`.

When converting dates in Bash, remember that the `date` command can work with the system's current timestamp or a provided date. Timezone handling is also crucial for accurate date conversions.

## See Also
- GNU coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Format specifiers for the date command: https://man7.org/linux/man-pages/man1/date.1.html
