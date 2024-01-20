---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means fetching specific parts of a string, based on start and end positions or patterns. We do it to manipulate and derive pertinent data from more extensive text information.

## How to:
Let's dive into examples in Fish Shell. 
Say we have a string `greet` with the value "Hello, welcome to Fish Shell tutorial!".

Define the string:
```Fish Shell
set greet "Hello, welcome to Fish Shell tutorial!"
```

Extract the substring "welcome to Fish Shell" using the classic `string sub`:
```Fish Shell
string sub -s 8 -l 23 -- $greet
```
Output:
```Fish Shell
welcome to Fish Shell
```

## Deep Dive
Historically, Fish Shell has borrowed techniques for string manipulations from legacy Unix tools, spawning a healthier and more modern alternative. 

Alternatives like native POSIX string manipulations in bash or zsh can often involve complex, convoluted syntax. In comparison, Fish strives for user-friendliness, providing a dedicated `string` built-in command with a range of sub-commands, including our focus `sub`.

Under the hood, the `string sub` operates by considering strings as arrays of characters. It takes a start index `-s` and a length `-l` to extract the required substring. Bear in mind, Fish Shell's indices are 1-based, unlike 0-based as in many other languages.

## See Also
Get more grip on Fish Shell Strings:
1. [Official Fish Documentation on Strings](https://fishshell.com/docs/current/cmds/string.html)
2. [Fish Shell String Operations](https://www.baeldung.com/linux/fish-shell-string-operations)
3. [Getting Started with Fish Shell](https://opensource.com/article/19/11/getting-started-fish-shell)