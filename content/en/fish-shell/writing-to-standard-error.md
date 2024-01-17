---
title:                "Writing to standard error"
html_title:           "Fish Shell recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error is a common practice in programming where error messages or debugging information are outputted. This is a way for developers to communicate with the user or other programs during the execution of a script. 

## How to:

```Fish Shell
echo "This is a sample error message" >&2
```

**Sample Output:**
```bash
This is a sample error message
```

```Fish Shell
printf "The value of x is %s\n" "$x" >&2
```

**Sample Output:**
```bash
The value of x is 10
```

## Deep Dive:

Writing to standard error has been a common practice in programming for many years. It allows developers to differentiate between error messages and regular output. This is especially useful when debugging a script or program, as error messages can help identify the source of the issue.

An alternative to writing to standard error is writing to a log file. However, this approach requires setting up a log file and constantly checking it for errors. Writing to standard error is a more immediate and convenient way to output error messages.

In implementation, writing to standard error is achieved by using the ">&2" notation after the message, which redirects the output to standard error instead of standard output.

## See Also:

For more information on writing to standard error, you can check out the official fish shell documentation: https://fishshell.com/docs/current/tutorial.html#tut_stderr. Additionally, you can also explore other shell scripting languages such as Bash or Zsh that also support writing to standard error.