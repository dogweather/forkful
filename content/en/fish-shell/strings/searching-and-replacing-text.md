---
date: 2024-01-20 17:57:35.968213-07:00
description: "Searching and replacing text is finding specific strings and swapping\
  \ them with something else. Programmers do it to update code, correct errors, or\
  \ to\u2026"
lastmod: '2024-03-13T22:45:00.458659-06:00'
model: gpt-4-1106-preview
summary: "Searching and replacing text is finding specific strings and swapping them\
  \ with something else. Programmers do it to update code, correct errors, or to\u2026"
title: Searching and replacing text
weight: 10
---

## What & Why?
Searching and replacing text is finding specific strings and swapping them with something else. Programmers do it to update code, correct errors, or to reformat data — it's a huge timesaver.

## How to:
Let's change all instances of 'cat' to 'dog' in a string.

```Fish Shell
echo "One cat, two cats, three cats." | string replace -a 'cat' 'dog'
```
Sample output:
```
One dog, two dogs, three dogs.
```
Replacing text in a file named `pets.txt`:

```Fish Shell
string replace -a 'cat' 'dog' < pets.txt > updated_pets.txt
```

Using variables for patterns:

```Fish Shell
set old "cat"
set new "dog"
string replace -a $old $new < pets.txt > updated_pets.txt
```

## Deep Dive
Search and replace has been in text editors since the early days. Think `sed` for stream editing in Unix — that’s old school cool. Fish takes this further, making it simpler with the `string` command. No more regex headaches unless you want them. Alternatives? Sure: `sed`, `awk`, Perl scripts, even `vim` macros. But Fish’s `string` command is elegant and less prone to errors for the common cases.

## See Also:
- Fish Shell’s official documentation on the `string` command: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed by Example, Part 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- AWK Language Programming — String Functions: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
