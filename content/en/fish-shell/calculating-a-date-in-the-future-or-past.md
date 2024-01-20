---
title:                "Calculating a date in the future or past"
html_title:           "Fish Shell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Fish Shell for Time Travel (Sort Of)
Step into the fascinating world of Fish Shell, where you can roam around dates past, present and future, like a time lord, but at a slightly lower pay scale. 

## What & Why?
Ever wondered, "What date will it be 100 days from now?" or "What's the date 50 days before March 30, 2025?" This is called calculating dates in the future or past. As a programmer, you'll do it, well, because software eats time for breakfast, and you want to stay ahead!

## How to:
In Fish Shell, using date function is the pathway to go:
```Fish Shell
date -v+30d
```
Easy, right? The `-v` option adjusts the specified date and `+30d` indicates 30 days in future.

To roll back to the past, just use a minus `-`:
```Fish Shell
date -v-30d
```
Print the date 30 days back.

And not just days, you can go full Hogwarts with weeks (`w`), months (`m`), years (`y`), hours (`H`), minutes (`M`), or seconds (`S`).
```Fish Shell
date -v+1y
date -v+2H
date -v-2M
```
This will print the date 1 year in future, 2 hours in future, and 2 minutes in past respectively.

## Deep Dive
The `date` function has been partying with Unix-like operating systems since the dawn of time (or 1970). It's got your back in all POSIX-compliant fellows, including our bright-eyed Fish Shell. 

Looking for alternatives? You've got yourself the `strftime` or the royal `datetime` module in Python, or JavaScript's `Date`, if you're into that sort of thing.

Diving deeper, `date -v` changes the date relative to the one it's given. When it's not given a date, it takes 'now'. Then it adjusts according to the `value`, where 'value' is an integer with an optional minus `-` sign for times in the past.

## See Also
1. [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
2. [Unix date man pages](https://man7.org/linux/man-pages/man1/date.1.html)
3. [Wikipedia: Unix Time](https://en.wikipedia.org/wiki/Unix_time) 

Feel free to lose yourself in the sands of time. Or at least, know how to calculate 'em.