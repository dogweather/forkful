---
title:                "Getting the current date"
date:                  2024-01-20T15:14:18.448352-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date means grabbing the present calendar date from your system. Programmers do this to timestamp events, schedule tasks, or just display the date for users.

## How to:

In Fish Shell, snagging the current date is a breeze. Use the `date` command:

```fish
# Get the current date in the default format
date

# Sample output
Wed Apr  5 15:26:42 PDT 2023

# Get the current date in a custom format, e.g., YYYY-MM-DD
date "+%Y-%m-%d"

# Sample output
2023-04-05
```

If you want to assign it to a variable, just do:

```fish
# Store the current date in a variable
set current_date (date "+%Y-%m-%d")

# Echo the variable
echo $current_date

# Sample output
2023-04-05
```

## Deep Dive

Historically, the `date` command comes from UNIX, and it has been around for decades. In Fish Shell, you're using a friendlier version of this ancient tool. The `%Y-%m-%d` format for the `date` command gives you the year, month, and day, but you've got a ton of other options like `%H` for hours or `%M` for minutes.

Why use Fish instead of Bash or Zsh for this? Well, Fish is known for its more straightforward, more readable syntax. For example, setting variables is a lot clearer (`set varname value` vs. `varname=value`), and you don't need to prefix with `$` when using them.

Alternatives to Fish's built-in `date` could involve installing more hefty tools like `GNU date` for more features or harnessing other Fish functions or even external programs if you need more custom behavior.

Implementation-wise, when you run `date` in Fish, you're using Fish's wrapper around the system's date command. That means on Linux, you're probably using `GNU date`, while on macOS, you're using the BSD version. They're pretty similar, but there are some subtle differences in the options they support.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
