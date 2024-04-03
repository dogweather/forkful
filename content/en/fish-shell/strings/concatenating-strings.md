---
date: 2024-01-20 17:34:46.244068-07:00
description: "Concatenating strings means sticking them together end-to-end. Programmers\
  \ do it to combine text, like building a full sentence from words or creating\u2026"
lastmod: '2024-03-13T22:45:00.464973-06:00'
model: gpt-4-1106-preview
summary: Concatenating strings means sticking them together end-to-end.
title: Concatenating strings
weight: 3
---

## What & Why?
Concatenating strings means sticking them together end-to-end. Programmers do it to combine text, like building a full sentence from words or creating file paths.

## How to:
In Fish, slap strings together with spaces between or use `string join`.

```fish
# Combine 'Hello' and 'World!' with a space
echo 'Hello' 'World!'

# Output: Hello World!

# Concatenate variables
set greet "Howdy"
set who "Partner"
echo $greet $who

# Output: Howdy Partner

# No-spaces concatenation with string join
set file "report"
set ext "txt"
string join '' $file '.' $ext

# Output: report.txt
```

## Deep Dive
Concatenation's been around since the dawn of programming. In Fish, `string join` is cleaner than older methods, such as using `echo` followed by string vars without quotes. This approach avoids subcommand overhead, which can be a performance win.

Alternatives include the use of `printf`, which gives more formatting control but is a touch more complex for simple joining operations. Example:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

Fish's `string` command is part of a built-in string manipulation toolbox introduced to make text processing more straightforward. It’s not unique to Fish, but its inclusion as a built-in tool keeps things simple.

## See Also
- Official Fish documentation: [link](https://fishshell.com/docs/current/cmds/string.html)
- Community tutorials: [link](https://fishshell.com/docs/current/tutorial.html#tutorial)
- Discussion on string manipulation in shells: [link](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)
