---
date: 2024-01-25 20:50:20.306001-07:00
description: "Bash doesn't come with a built-in debugger like some other languages,\
  \ but you can use built-in commands like `set -x` to trace what's happening. Or,\
  \ for\u2026"
lastmod: '2024-03-13T22:45:00.248880-06:00'
model: gpt-4-1106-preview
summary: "Bash doesn't come with a built-in debugger like some other languages, but\
  \ you can use built-in commands like `set -x` to trace what's happening. Or, for\u2026"
title: Using a debugger
weight: 35
---

## How to:
Bash doesn't come with a built-in debugger like some other languages, but you can use built-in commands like `set -x` to trace what's happening. Or, for an upgrade, there's `bashdb`, a proper debugger to step through your code. Here's a peek:

```Bash
# Using set -x to debug
set -x
echo "Start debugging"
my_var="Hello, Debugging World!"
echo $my_var
set +x

# Using bashdb
# Install bashdb with your package manager, e.g., apt, yum, brew.
# Debug a script called my_script.sh:
bashdb my_script.sh
```

Output when running with `set -x`:
```Bash
+ echo 'Start debugging'
Start debugging
+ my_var='Hello, Debugging World!'
+ echo 'Hello, Debugging World!'
Hello, Debugging World!
+ set +x
```

## Deep Dive
Historically, debugging Bash scripts meant littering your code with `echo` statements. But then came `set -x`, giving us a peek into the runtime execution without manual printouts. And for those craving more control, the `bashdb` debugger popped up, inspired by the gdb debugger for C/C++.

As for alternatives, beyond the `set` commands (`-x`, `-v`, `-e`), other options include redirecting output to a file for analysis or using external tools like ShellCheck for static analysis.

Implementation-wise, `set -x` is easy; it's a native Bash option that prints commands and their arguments as they're executed. `bashdb`, on the other hand, allows stepping through code, setting breakpoints, and evaluating expressions - things that give you a fighting chance against more elusive bugs.

## See Also
- Bash Debugger Project: http://bashdb.sourceforge.net/
- "Pro Bash Programming" by Chris Johnson and Jayant Varma for advanced scripting.
- ShellCheck for static analysis: https://www.shellcheck.net/
