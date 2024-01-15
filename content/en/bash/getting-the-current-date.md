---
title:                "Getting the current date"
html_title:           "Bash recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Do you ever need to quickly know the current date? Maybe you need to keep track of project timelines or simply curious about the day. With Bash, you can easily retrieve the current date with a single command.

## How To

```Bash
date
```

Running this command will output the current date and time in the format of "Day of the Week Month Day HH:MM:SS Timezone Year". For example:

```
Wed May 19 10:24:57 UTC 2021
```

If you want to customize the date and time format, you can use the `date` command with the `+` option and specify the format you want. Here are a few examples:

- To display only the current date in YYYY-MM-DD format:

```Bash
date +%F
```

Output: `2021-05-19`

- To display only the current time in HH:MM:SS format:

```Bash
date +%T
```

Output: `10:24:57`

- To display the current date and time in a custom format:

```Bash
date +"Today is %A, %B %d, %Y. The time is %I:%M %p."
```

Output: `Today is Wednesday, May 19, 2021. The time is 10:24 AM.`

For a full list of available date and time format options, check out the `date` command's manual page by running `man date` in the terminal.

## Deep Dive

The `date` command retrieves the current date and time from your system's hardware clock. This clock keeps track of the time, even when the system is turned off. When you turn on your system, the hardware clock's time is used to set the system's clock.

You can update the hardware clock's time and, by extension, your system's time, by using the `hwclock` command. This command requires root privileges, so you'll need to use `sudo` before the command.

To set the hardware clock to the current system time, run:

```Bash
sudo hwclock --systohc
```

If you're using a server or virtual machine hosted in a different timezone, you can specify the timezone for the `hwclock` command using the `--utc` option. For example, to set the hardware clock to UTC time, run:

```Bash
sudo hwclock --systohc --utc
```

## See Also

- `man date`: Displays the `date` command's manual page for more detailed information.
- `man hwclock`: Displays the `hwclock` command's manual page for more detailed information.
- [Bash Beginner's Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/): A comprehensive guide to learning Bash scripting.
- [Bash Cheat Sheet](https://devhints.io/bash#dates-and-times): A quick reference guide for Bash commands and syntax.