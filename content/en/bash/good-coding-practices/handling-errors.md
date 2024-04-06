---
date: 2024-01-21 21:19:09.389800-07:00
description: 'How to: Sample output when an error occurs.'
lastmod: '2024-04-05T21:53:35.948721-06:00'
model: gpt-4-1106-preview
summary: Sample output when an error occurs.
title: Handling errors
weight: 16
---

## How to:
```Bash
#!/bin/bash

# Redirecting stderr to a file
grep "something" file.txt 2> errors.log

# Error handling with exit statuses
if ! grep "something" file.txt; then
    echo "Oops, something went wrong searching for 'something'."
    exit 1
fi

# Using a trap to clean up before exiting on error
cleanup() {
  echo "Cleaning up temporary files..."
  rm temp_*
}

trap cleanup ERR

# intentional error: file doesn't exist
cat temp_file.txt
```

Sample output when an error occurs:

```
Cleaning up temporary files...
cat: temp_file.txt: No such file or directory
```

## Deep Dive
Error handling in Bash scripting dates back to the origins of the Unix shell, where robust and reliable scripts were (and are) vital for system administration and automation. Traditionally, errors in Bash are handled by checking the exit status of a command, which by convention returns 0 for success and a nonzero value for failure.

Bash introduced the `trap` command as a built-in, allowing users to specify commands to run on various signals or script exits. This is useful for cleanup tasks or a last resort error handling mechanism.

There's also the `set` command, which can change the behavior of Bash on errors. For example, `set -e` will make a script exit immediately if any command exits with a nonzero status, a way to fail fast and avoid cascading errors.

Alternatives to Bash built-in error handling include explicitly checking for the existence of files, using command substitution, or even writing your own functions to handle errors more granularly.

Though rigorous error handling can sometimes feel overkill for small scripts, it's a practice that can save a lot of time debugging and prevent unexpected behavior for both you and the users.

## See Also
- Bash Manual on Shell Parameters: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Advanced Bash-Scripting Guide's section on Error Handling: https://www.tldp.org/LDP/abs/html/exit-status.html
- An in-depth guide to `trap`: https://mywiki.wooledge.org/SignalTrap

Remember, scripting is an art form, and how you handle the slips and stumbles can make your masterpiece more resilient. Happy scripting!
