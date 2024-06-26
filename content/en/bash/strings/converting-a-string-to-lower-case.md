---
date: 2024-01-20 17:37:41.148972-07:00
description: 'How to: Here''s the simple way to convert a string to lower case in
  Bash.'
lastmod: '2024-03-13T22:45:00.231474-06:00'
model: gpt-4-1106-preview
summary: Here's the simple way to convert a string to lower case in Bash.
title: Converting a string to lower case
weight: 4
---

## How to:
Here's the simple way to convert a string to lower case in Bash:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

Output:

```
make me lower case
```

Bash 4.0 and above has a built-in way with parameter expansion:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

Output:

```
make me lower case
```

## Deep Dive
Before Bash 4.0, commonly used methods to convert strings to lower case involved external utilities like `tr`, `awk`, or `sed`. Each of these provide different ways to manipulate strings beyond simply changing case, but may need to spawn a new process, affecting performance.

The introduction of `${parameter,,pattern}` syntax in Bash 4.0 provided a native feature to transform strings which is faster and doesn't rely on external utilities. There are alternatives within Bash itself:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - as shown above.

In terms of implementation, `${parameter,,pattern}` don't just alter ASCII characters; they're UTF-8 aware and can handle non-English characters, making them versatile for international applications.

## See Also
- Bash Parameter Expansion: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- `tr` Command: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK Programming: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` Stream Editor: https://www.gnu.org/software/sed/manual/sed.html
