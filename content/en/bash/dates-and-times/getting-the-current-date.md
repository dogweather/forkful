---
date: 2024-02-03 19:02:32.872057-07:00
description: "Retrieving the current date in Bash involves using built-in commands\
  \ to display the date and time in various formats. Programmers use this functionality\u2026"
lastmod: '2024-03-11T00:14:34.120813-06:00'
model: gpt-4-0125-preview
summary: "Retrieving the current date in Bash involves using built-in commands to\
  \ display the date and time in various formats. Programmers use this functionality\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?
Retrieving the current date in Bash involves using built-in commands to display the date and time in various formats. Programmers use this functionality for tasks such as timestamping logs, scheduling tasks, or just as part of their system information scripts to track when actions were performed.

## How to:
In Bash, the `date` command is your primary tool for getting the current date and time. Here are a few examples of how to use it:

1. **Get the current date and time in the default format:**

```bash
date
```

*Sample output:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Customize the output format:** You can specify the output format using `+%` format specifiers. For example, to display the date in YYYY-MM-DD format:

```bash
date "+%Y-%m-%d"
```

*Sample output:*
```
2023-04-05
```

3. **Get the current UNIX timestamp:** The UNIX timestamp is the number of seconds since the Unix Epoch (January 1, 1970). This is useful for scripts that perform calculations based on time differences.

```bash
date "+%s"
```

*Sample output:*
```
1672877344
```

No popular third-party libraries are typically used for this basic operation in Bash as the built-in `date` command provides comprehensive functionality. However, for more advanced date and time manipulations, programmers might use other programming languages or tools that offer libraries for date arithmetic and parsing, such as Python's `datetime` module.
