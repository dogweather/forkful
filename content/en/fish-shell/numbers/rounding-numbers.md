---
title:                "Rounding numbers"
aliases:
- /en/fish-shell/rounding-numbers/
date:                  2024-01-25T02:59:33.441599-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers is about chopping off decimal places to simplify your data or fit specific formats. Programmers do it for user-friendly display, efficient storage, or when decimal precision is a non-issue.

## How to:
In Fish, rounding numbers hinges on the `math` command. Use `math -s0` to round to the nearest integer.

```fish
# Round up
echo (math -s0 "4.7")
# Output: 5

# Round down
echo (math -s0 "4.3")
# Output: 4

# Round to two decimal places
echo (math -s2 "4.5678")
# Output: 4.57

# Round negative number
echo (math -s0 "-2.5")
# Output: -3
```

## Deep Dive
Historically, rounding numbers was done more manually or with external tools, but in modern shells like Fish, it's baked into built-in utilities. Fish's approach using the `math` command simplifies things compared to older shells. Alternatives in other programming environments vary; languages like Python use functions like `round()`, while Bash might require more complex expressions or `bc` utility. Fish's rounding implementation simplifies scripting by keeping the math inside the shell environment instead of invoking other tools or languages.

## See Also
- Fish documentation for the `math` command: https://fishshell.com/docs/current/cmds/math.html
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): https://ieeexplore.ieee.org/document/4610935
