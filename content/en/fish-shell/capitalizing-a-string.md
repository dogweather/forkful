---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means turning all the characters into uppercase. Programmers do this for consistency, readability, or to meet certain coding standards.

## How to:
In Fish, you capitalize a string with the `string upper` command. Here's how you do it:

```Fish Shell
set lowercased "fish shell is fun"
set capitalized (string upper $lowercased)
echo $capitalized
```

Output:
```
FISH SHELL IS FUN
```

## Deep Dive
Historically, capitalizing strings has been used in programming for formatting outputs, storing data uniformly, and for case-insensitive comparisons. While Fish Shell is relatively young, its string manipulation functions draw inspiration from other Unix shells, delivering more readable syntax and convenience.

Key points in Fish's design philosophy include being user-friendly and providing functions that do what you'd expect, hence the straightforward `string upper` command. Earlier shells would require you to pipe echo commands to `tr` or use the likes of `awk` for such an operation, which can be less intuitive for casual users.

Alternatives include using `awk`:
```Fish Shell
echo "fish shell is fun" | awk '{print toupper($0)}'
```

Or `tr`:
```Fish Shell
echo "fish shell is fun" | tr '[:lower:]' '[:upper:]'
```

Despite these alternatives, `string upper` in Fish is clear and to the point, avoiding Unix's historical baggage of cryptic command options and syntax. Capitalizing a string in Fish doesn't change the original string unless you explicitly reassign it, which protects your data from accidental mutations.

## See Also
- Fish documentation on string manipulation: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- A brief history of Unix text-processing tools: [Unix Text Processing (O'Reilly)](http://www.oreilly.com)
- A guide to string manipulation in Unix shells for comparison: [Greg's Wiki (mywiki.wooledge.org)](http://mywiki.wooledge.org/BashFAQ/099)
