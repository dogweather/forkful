---
date: 2024-02-03 19:02:33.004483-07:00
description: "How to: Fish Shell utilizes external commands like `date` for getting\
  \ the current date, offering flexibility to format the output as needed. Here's\
  \ how to\u2026"
lastmod: '2024-03-13T22:45:00.483867-06:00'
model: gpt-4-0125-preview
summary: Fish Shell utilizes external commands like `date` for getting the current
  date, offering flexibility to format the output as needed.
title: Getting the current date
weight: 29
---

## How to:
Fish Shell utilizes external commands like `date` for getting the current date, offering flexibility to format the output as needed. Here's how to use it:

```fish
# Display the current date in the default format
echo (date)

# Output example: Wed 25 Oct 2023 15:42:03 BST
```

To customize the format of the date, you can use the `+` option followed by format specifiers:

```fish
# Display the current date in YYYY-MM-DD format
echo (date "+%Y-%m-%d")

# Output example: 2023-10-25
```

For more complex tasks, such as working with timestamps or performing date arithmetic, Fish Shell relies on external tools like `date` due to its scripting nature. Here's an example of getting the current UNIX timestamp:

```fish
# Get the current UNIX timestamp
echo (date "+%s")

# Output example: 1666710123
```

And to add one day to the current date using `date`:

```fish
# Add one day to the current date
echo (date -d "+1 day" "+%Y-%m-%d")

# Output example: 2023-10-26
```

Note: The examples use `date` command options that work with GNU coreutils. Options may vary in other environments like macOS, which uses BSD date command by default. Always refer to `date --help` or the man page for details specific to your environment.
