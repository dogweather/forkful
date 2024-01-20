---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Turning The Heat Down: A Guide to Converting Strings to Lower Case in Bash

## What & Why?

Turning a string to lower case means changing all upper-case letters in it to lower-case ones. Bash programmers often do this for consistency, as case-sensitive processes can trip up on "APPLE" versus "apple".

## How to:

Simple lowercase conversion in Bash is a one-liner.

```Bash
string="My STRing"
echo "${string,,}"
```

The output will be:

```Bash
my string
```
## Deep Dive

Bash 4.0 (released in 2009) first introduced parameter expansion enhancements that made it much easier to modify string case. Pre-Bash 4.0 scripts needed external programs like `tr`.

Alternatives? You could call in other languages if Bash can't fit the bill.

Here's a Python example:

```Bash
string="My STRing"
echo $string | python3 -c "print(input().lower())"
```

And a Perl one:

```Bash
string="My STRing"
echo $string | perl -ne 'print lc'
```

Translation? `'print lc'` tells Perl: "print the lower case version."

But mind the speed impact. It's slight but can add up over many lines/operations. Bash's integrated string case-modifying is generally the fastest.

Also note, some Unix-like systems may not include Python, Perl, or even new versions of Bash. Make sure to check, but expect latest versions on any recent Linux distro.

## See Also

- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Python Lower Function](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Perl lc Function](https://perldoc.perl.org/functions/lc.html)
- [AWK Guide](https://www.gnu.org/software/gawk/manual/gawk.html) for another alternative.