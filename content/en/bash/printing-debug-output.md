---
title:                "Printing debug output"
date:                  2024-01-20T17:51:53.543395-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is all about echoing data to the console to check what's going on in your script. Programmers do it to track variables, follow logic flow, and spot the pesky bugs.

## How to:

```Bash
#!/bin/bash

# Define a variable
name="Gizmo"

# Print variable for debugging
echo "Debug: The variable name is $name"

# Conditional with debug output
if [[ $name == "Gizmo" ]]; then
    echo "Debug: Entered the if-statement."
    # Do something
fi

# Loop with debug output
for i in {1..3}; do
    echo "Debug: Loop iteration $i"
    # Do something in loop
done
```

Output:
```
Debug: The variable name is Gizmo
Debug: Entered the if-statement.
Debug: Loop iteration 1
Debug: Loop iteration 2
Debug: Loop iteration 3
```

## Deep Dive

Originally, debugging meant removing physical bugs that disrupted early computers. Nowadays, it's about squashing code bugs. Debug outputs are the programmer's magnifying glass. 

Alternatives to `echo` in bash scripts include `printf` for more formatting options or writing to a file with redirection `>` for persistent logs.

Bash also supports conditional debug output with the built-in `set -x` to trace commands and their arguments as they’re executed. `set -x` is great for full-script debugging.

## See Also

- Bash's `man` page: `man bash`
- Advanced scripting guide: [Bash Guide for Beginners by Machtelt Garrels](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Stack Overflow for troubleshooting: [stackoverflow.com](https://stackoverflow.com/questions/tagged/bash)
