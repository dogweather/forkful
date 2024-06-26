---
date: 2024-01-20 17:45:22.688050-07:00
description: 'How to: In Fish, you use `string` command to manipulate strings. Here''s
  how.'
lastmod: '2024-03-13T22:45:00.462397-06:00'
model: gpt-4-1106-preview
summary: In Fish, you use `string` command to manipulate strings.
title: Extracting substrings
weight: 6
---

## How to:
In Fish, you use `string` command to manipulate strings. Here's how:

### Grab from start:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -l 4 # Outputs 'Fish'
```

### Snip from end:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s -4 # Outputs 'fun!'
```

### Specific range:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s 6 -l 5 # Outputs 'Shell'
```

## Deep Dive
In the old days, we'd slice and dice strings in Fish using external tools like `cut`, `awk`, or `sed`. Now, `string` is our go-to built-in function introduced in Fish 2.3.0. It's faster, more readable, and integrates seamlessly with our scripts.

`string sub` isn't your only option. Other `string` functions can split strings, replace parts, or join them. This focuses on minimal resource use and ease of understanding.

Regarding implementation, when you extract substrings, Fish reads the string and outputs just the part you've specified, all while respecting character encoding and avoiding common bugs in substring extraction, like splitting a character in half.

## See Also
- Official Fish documentation on `string`: https://fishshell.com/docs/current/cmds/string.html
- Community tutorials on Fish scripting: https://fishshell.com/docs/current/tutorial.html
- Stack Overflow discussions on Fish string manipulation: https://stackoverflow.com/questions/tagged/fish
