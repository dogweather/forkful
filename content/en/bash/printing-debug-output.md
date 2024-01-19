---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debug Debug: Print Debug Output In Bash

## What & Why?
Printing debug output means rendering textual information at runtime for possible errors. This helps verify your program works as expected. Ever forgotten a semi-colon and gone mad looking for it? Debug output saves you time and aspirin.

## How to:
In Bash, use `echo` or `printf`. They’re simple, but powerful.

```Bash
# Test script with echo
echo "Start of script"
```

Output:
```Bash
Start of script
```

Or, for more control, `printf`. Don't forget the new line `\n`.

```Bash
# Test script with printf
printf "Start of script\n"
```

Output:
```Bash
Start of script
```

## Deep Dive
The `echo` command was present in the earliest versions of Unix. `printf` showed up in Unix V7 and is recommended nowadays due to stricter standards-compliant behavior.

Alternatives? `print` in Korn shell but it's non-standard. Stick with `printf`.

Under the hood, printing debug output doesn't happen in a vacuum: it writes to the standard output (stdout) file descriptor. Neat, huh?

## See Also
- Check `man echo` and `man printf` for all the nitty-gritties.
- For more on file descriptors, see: [Understanding Linux File Descriptors](https://www.linuxjournal.com/article/10681).