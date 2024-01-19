---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Bash Guide: Deleting Characters Matching a Pattern

## What & Why?

Deleting characters matching a pattern is just like using a super-powered eraser, wiping off particular sequences or types from your bash strings. Programmers do it for text mining, reformatting data, or just for cleaner, clinker-free code.

## How to:

Let's use the built-in `tr` command, meaning translate or delete:

```Bash
# Remove all digits from a string
echo '123 Hello, World!!456' | tr -d '0-9'
# Output: ' Hello, World!!'
```

Or with variable:

```Bash
phrase='123 Hello, World!!456'
clean=${phrase//[0-9]/}
echo $clean
# Output: ' Hello, World!!'
```

This will also work for specific pattern, say goodbye to a, b, and c:

```Bash
echo 'abc Hello, World!!' | tr -d 'abc'
# Output: ' Hello, World!!'
```

## Deep Dive

Delving back, `tr` was from Unix's early days, around 1970. An oldie but a goodie, it's simpler than regex but not as finessed.

Built-in bash replacement, like `${phrase//pattern/}`, offers similar results with regex-like power. Now, it doesn't run another process like `tr` does, it's all in the bash shell, making it a smidge quicker.

For character removal, you've got a slew of options across the bash/Unix/Linux ecosystem: `sed`, `awk`, `perl`, etc.

## See Also

- Linux `tr` command [Tutorial](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- `tr` vs `tr -d` [Stackoverflow Post](https://stackoverflow.com/questions/4546124/how-does-the-tr-command-work)
- Bash `Parameter Expansion` [Guide](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)