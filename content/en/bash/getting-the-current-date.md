---
title:                "Getting the current date"
date:                  2024-01-20T15:12:56.837543-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in bash is about getting the system's date and time. Programmers need it for logging, time-stamping, or scheduling tasks.

## How to:
To fetch the current date and time, you use `date`. Here's the simple way:

```Bash
date
```

And boom, you get something like this:

```
Mon Mar 27 12:45:21 PDT 2023
```

Need the date in a different format? No problemo. Use `+%` options:

```Bash
date +"%Y-%m-%d"
```

Output's all neat and tidy now:

```
2023-03-27
```

## Deep Dive
Back in the day, systems didn't always have internal clocks. Hence, folks relied on time-sharing systems to get the time. Today, every system you run Bash on knows the time. Thank `date`.

`date` is versatile. Wanna get the next week's date? Just add a fancy `--date` flag:

```Bash
date --date="next week"
```

But wait, there's more! Got a different time zone on your mind?

```Bash
TZ="Europe/Paris" date
```

Now you're getting Paris time. Fancy.

Bash isn't alone in the date-getting game. Python, PHP, JavaScript – they all have their own ways. But in the realm of shell scripting, `date` is your trusty sidekick.

Why does this matter? Automation, my friend. Scripts that do things depending on the date and time rely on `date`. Cron jobs? They love a good timestamp.

Here's the technical rundown: `date` pulls info from the system clock, which gets synchronized to hardware or network time sources, so you're not living in the past.

## See Also
- Check out `man date` for an exhilarating read on all things `date`.
- Swing by Greg's Wiki for some bashful scripting tips: http://mywiki.wooledge.org/BashGuide
- If you're itching for more, there's always the GNU coreutils manual: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
