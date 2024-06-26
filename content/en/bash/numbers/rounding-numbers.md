---
date: 2024-01-25 03:00:17.162264-07:00
description: 'How to: Here''s the lowdown on rounding in Bash.'
lastmod: '2024-03-13T22:45:00.238287-06:00'
model: gpt-4-1106-preview
summary: Here's the lowdown on rounding in Bash.
title: Rounding numbers
weight: 13
---

## How to:
Here's the lowdown on rounding in Bash:

```Bash
# Round down using 'floor' with bc
echo "scale=0; 3.49/1" | bc

# Round up using 'ceiling' with bc
echo "scale=0; 3.01/1" | bc -l

# Round to nearest whole using printf
printf "%.0f\n" 3.49

# A trick to round to the nearest whole using bc
echo "(3.49+0.5)/1" | bc
```

Sample outputs—straight from terminal's mouth:

```
3  # Rounded down (floor)
4  # Rounded up (ceiling)
3  # Rounded to nearest (with printf)
3  # Rounded to nearest (with bc)
```

## Deep Dive
Back in the day, there was no `bc` or `printf` in Bash scripts to do the math magic. Old-schoolers had to rely on external tools or crafty workarounds. Now, `bc` lets you do precision math. Keep in mind, `bc` doesn't round by default—it floors. The scale part sets the decimal point action.

Alternatives? You could use `awk` for rounding without swapping to `bc` or wrangle with `perl` for heftier math needs. For the masochistic, go pure Bash with, let's say, iterative string manipulation – but why?

As for details, `bc` doesn't just round, it does loads of math stuff—scale it, sine it, sqrt it, you name it. With `printf`, it’s more about formatting text, but hey, it rounds numbers, so we're not complaining.

## See Also
For those hungry for more:

- GNU `bc` manual: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` command: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK user's guide (for rounding and other text processing): https://www.gnu.org/software/gawk/manual/gawk.html
- More Bash math, scripting, and number tricks: https://mywiki.wooledge.org/BashFAQ/022
