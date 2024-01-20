---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Grabbing the current date is a way programmers capture the system's current day, month, and year. This is handy for logging, generating reports, timestamps, and many other time-based functionalities.

## How to:

To get the current date in Fish Shell (the currents version), use the `date` command. 

```fish
set current_date (date)
echo $current_date
```

This will print the current date, something like:

```fish
Thu Sep 30 21:31:01 UTC 2021
```

For more granularity and formatting options, pass arguments to the `date` command. For instance, to get just the day:

```fish
set day (date +%A)
echo $day
```

That typically generates:

```fish
Thursday
```

## Deep Dive

Fish (Friendly interactive shell) is a Unix shell that focuses on user-friendliness and interactivity. The ability to fetch the current date isn't new or unique to Fish, similar functionality can be seen in older shells like Bash or Zsh.

There are a few alternatives to the `date` command approach depending on your needs. For instance, the `strftime` function in a language like C, which gives more control over the output format. Alternatively, higher-level languages such as Python, Java, and JavaScript provide their own methods to fetch the current date.

Under the hood, when you run the `date` command, Fish is calling a system function that interfaces with the operating system to pull the current date and time. It then formats this information based on the provided format strings, like `%A` for the day of the week.

## See Also

To dive deeper into the date command and the Fish shell coding, check out these resources:

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)