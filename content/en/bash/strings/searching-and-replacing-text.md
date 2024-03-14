---
date: 2024-01-20 17:57:00.215150-07:00
description: "Searching and replacing text in bash means swapping out words or patterns\
  \ in a string or file with something else. It's a day-to-day task for cleaning\u2026"
lastmod: '2024-03-13T22:45:00.229850-06:00'
model: gpt-4-1106-preview
summary: "Searching and replacing text in bash means swapping out words or patterns\
  \ in a string or file with something else. It's a day-to-day task for cleaning\u2026"
title: Searching and replacing text
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text in bash means swapping out words or patterns in a string or file with something else. It's a day-to-day task for cleaning data, fixing code, or automating edits.

## How to:
Here's how you wield the power of search and replace in bash:

1. Swap text within a string using `sed`:
```Bash
echo "Hello world" | sed 's/world/universe/'
# Output: Hello universe
```

2. Replace text in a file, saving the changes:
```Bash
sed -i 's/old_text/new_text/g' file.txt
```

3. Use variables in your search and replace:
```Bash
old="apple"
new="banana"
sed "s/$old/$new/g" <<< "I like apple pies"
# Output: I like banana pies
```

Remember, `g` at the end means "global", so you change every match in the line, not just the first one.

## Deep Dive

We've had tools for text processing on Unix-like systems for ages. `sed`, short for Stream Editor, is one such tool, and it's been around since the 1970s. It's not just for simple replacements; `sed` can slice and dice text in complex patterns too.

Alternatives? Sure. `awk` is a bit more advanced and can work wonders with columns and rows. For quick fixes, `grep` can help you find things, but it won't replace – it's more like the lookout.

Under the hood, `sed` uses regular expressions, which are like wildcards on steroids. They can match almost any pattern you can think of. It makes `sed` incredibly powerful, but also a bit tricky to master.

## See Also
- `man sed` for the manual on `sed`
- [An introduction to `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions for Beginners](https://www.regular-expressions.info/tutorial.html)
- The Art of Command Line for more bash tricks (https://github.com/jlevy/the-art-of-command-line)
