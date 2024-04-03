---
date: 2024-01-20 17:36:29.222778-07:00
description: "Converting a date into a string means changing the date's format from\
  \ its raw or timestamp form into a human-readable sequence of characters. Programmers\u2026"
lastmod: '2024-03-13T22:45:00.484732-06:00'
model: gpt-4-1106-preview
summary: Converting a date into a string means changing the date's format from its
  raw or timestamp form into a human-readable sequence of characters.
title: Converting a date into a string
weight: 28
---

## How to:
Fish shell keeps things straightforward. Let's format the current date:

```fish
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
```

Sample output:
```
2023-04-11
```

Want something more specific, like the day of the week?

```fish
set day_of_week (date "+%A")
echo $day_of_week
```

Sample output:
```
Tuesday
```

How about we add the time? Here's the date and time in a 24-hour format:

```fish
set date_and_time (date "+%Y-%m-%d %H:%M:%S")
echo $date_and_time
```

Sample output:
```
2023-04-11 21:30:47
```

## Deep Dive
In the past, Unix-like systems such as Linux adopted the `date` command, which has evolved over time and remains prevalent in shells like bash and zsh. Fish shell inherits this but encourages a more readable, flag-less syntax for setting variables.

There are alternatives, such as the `strftime` function in many programming languages. Fish doesn't natively support this, but `date` in UNIX is versatile enough to cover most needs.

When converting a date to a string, the format specifiers, like `%Y` for the year or `%A` for the weekday, follow the POSIX standard. The `date` command uses these specifiers to extract and format specific parts of the date.

It's important to note that, because dates and times are so locale and timezone dependent, the strings produced can vary unless specified. You can set the timezone before invoking `date`:

```fish
set TZ 'America/New_York'
set date_with_timezone (date "+%Y-%m-%d %H:%M:%S %Z")
echo $date_with_timezone
```

This ensures you've considered the locality of your data—a detail not to skim over in a globalized world.

## See Also
- The `man` page for `date` ([online manual](https://linux.die.net/man/1/date)) gives you the full scoop on format specifiers.
- For broader context, read about [POSIX standards](https://en.wikipedia.org/wiki/POSIX).
- Check out Fish shell's official documentation on [variables](https://fishshell.com/docs/current/language.html#variables) to understand the `set` command better.
