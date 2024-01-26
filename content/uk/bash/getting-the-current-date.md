---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:12:55.289412-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Що і Навіщо?

Getting the current date in Bash lets you stamp when events happen in scripts. It's useful for logging, backups, and any time-sensitive tasks.

## How to:
Як це зробити:

To get the current date and time, you just use `date`. Here’s a quick example:

```Bash
date
```

Output might look like this:

```
Tue Mar 14 17:22:36 EET 2023
```

Get the date in `YYYY-MM-DD` format:

```Bash
date +%F
```

Output:

```
2023-03-14
```

Want just the year?

```Bash
date +%Y
```

Output:

```
2023
```
  
## Deep Dive
Поглиблене Вивчення:

In older scripts, you might see `date` commands with backticks (`). It’s an old way to capture command output. Now we use `$(...)`. Like `$(date)`. Why? It's more readable and nests better.

Alternatives? You bet. For complex needs, people use `date` with options, like `date +%Y-%m-%d-%H:%M:%S` for a full timestamp.

Implementation detail: `date` reads system time. Setting the system time correctly, often via NTP (Network Time Protocol), thus matters.

## See Also
Дивись Також:

For more on `date`:

- Bash Manual: https://www.gnu.org/software/bash/manual/
- `man date` or `info date` in the terminal.

Interested in scripts and scheduling?

- Cron Jobs: https://opensource.com/article/17/11/how-use-cron-linux
- `at` and `batch` commands: https://linux.die.net/man/1/at
