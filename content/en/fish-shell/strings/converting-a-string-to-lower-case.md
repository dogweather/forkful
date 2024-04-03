---
date: 2024-01-20 17:38:08.868799-07:00
description: "Converting a string to lower case changes all the letters in that string\
  \ to their lower case form. Programmers do it for consistency, comparison, sorting,\u2026"
lastmod: '2024-03-13T22:45:00.460383-06:00'
model: gpt-4-1106-preview
summary: Converting a string to lower case changes all the letters in that string
  to their lower case form.
title: Converting a string to lower case
weight: 4
---

## What & Why?

Converting a string to lower case changes all the letters in that string to their lower case form. Programmers do it for consistency, comparison, sorting, or to meet case-sensitivity requirements of some systems. 

## How to:

Using the `string` command, converting text to lower case is straightforward. Just do:

```Fish Shell
echo "MAKE ME LOWERCASE" | string lower
```

Sample output:

```
make me lowercase
```

For a variable:

```Fish Shell
set my_string "SHOUTY CASE TEXT"
string lower -q -- $my_string
```

Output:

```
shouty case text
```

## Deep Dive:

Before Fish Shell, Unix users often used `tr '[:upper:]' '[:lower:]'` or `awk '{print tolower($0)}'`. While these work, they're not as clean or straightforward as Fish's built-in `string lower` function.

Fish introduced `string` in v2.3.0 (May 2016), elevating string manipulation to be a core part of the shell, rather than requiring external commands. This added simplicity and speed to common tasks like case conversion.

Why not just use `tr` or `awk`? `string lower` is built into Fish, meaning it's faster (no spawning new processes) and works in a consistent and predictable manner across different systems. It's also part of a broader `string` command suite that handles other string operations, which can make script writing tidier and more efficient.

## See Also:

- Official documentation for `string`: https://fishshell.com/docs/current/cmds/string.html
- Fish Shell GitHub repository: https://github.com/fish-shell/fish-shell
- The historical context and comparison of `string` vs traditional Unix commands: https://github.com/fish-shell/fish-shell/issues/159
