---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex, are patterns that describe sets of strings. Programmers use them to search, match, and manipulate text — super handy for finding needles in data haystacks.

## How to:
The Fish Shell has built-in regex support in commands like `string`. Let's dive into some examples:

**Basic Search:**

Find if "fish" is in the string:

```fish
echo "I love to fish for fish in my fish tank" | string match -r "fish"
```

Output:

```
fish
fish
fish
```

**Capture Groups:**

Extract matched groups using parentheses:

```fish
echo "Color: Blue, Code: #0000FF" | string match -r "Color: (\w+)"
```

Output:

```
Color: Blue
Blue
```

**Replace Text:**

Swap "fish" with "shark":

```fish
echo "One fish, two fish, red fish, blue fish" | string replace -ar "fish" "shark"
```

Output:

```
One shark, two shark, red shark, blue shark
```

## Deep Dive:
Regular expressions hail from theoretical computer science, concocted in the 1950s. Alternatives? Sure, you've got simple string searches or parsers for more structure, but regex is sweet for quick and dirty tasks. The Fish Shell uses PCRE (Perl Compatible Regular Expressions) under the hood, ensuring a robust set of features for pattern matching.

## See Also:
- Official Fish Shell documentation: [The string Command](https://fishshell.com/docs/current/cmds/string.html)
- Regex tutorial for beginners: [Regular Expressions 101](https://regex101.com/)
- In-depth understanding: [Mastering Regular Expressions by Jeffrey Friedl](http://shop.oreilly.com/product/9780596528126.do)
