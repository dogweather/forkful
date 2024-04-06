---
date: 2024-01-20 17:42:13.044712-07:00
description: "How to: In Fish Shell, the magic happens with the `string` utility,\
  \ a handy built-in tool for string operations - introduced in version 2.3.0. Prior\
  \ to\u2026"
lastmod: '2024-04-05T21:53:36.157851-06:00'
model: gpt-4-1106-preview
summary: In Fish Shell, the magic happens with the `string` utility, a handy built-in
  tool for string operations - introduced in version 2.3.0.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
```Fish Shell
# Remove digits from a string
set string "Fish123Shell"
echo $string | string replace -ra '[0-9]' ''
# Outputs: FishShell

# Strip out everything but lowercase letters
set noisy_string "F!i@s#h$%S^h&e*l(l)__+"
echo $noisy_string | string match -r '[a-z]+'
# Outputs: ishhell
```

## Deep Dive
In Fish Shell, the magic happens with the `string` utility, a handy built-in tool for string operations - introduced in version 2.3.0. Prior to this, users would fall back on UNIX staples like `sed` or `awk`. Why the change? Simplicity and integration. Having an in-house solution streamlines string manipulation, making scripts more readable and maintainable.

Alternatives? Sure, the old guard `sed` can still do the job:

```Fish Shell
set old_school_string "Fish@Shell2023"
echo $old_school_string | sed 's/[0-9]//g'
# Outputs: Fish@Shell
```

But why not leverage Fish’s own tools? For implementation, `string replace` has a `-r` option enabling regex patterns. `-a` applies the command to all matches, and adding a '' at the end tells it to replace with nothing, i.e., delete. Use `string match` when searching for a pattern to keep, instead of what to ditch.

## See Also
- Official Fish Shell Documentation on `string`: https://fishshell.com/docs/current/cmds/string.html
- Regex tutorial for deep diving into patterns: https://www.regular-expressions.info/
- Sed & Awk, age-old text powers: an intro: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html
