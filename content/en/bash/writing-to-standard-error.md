---
title:    "Bash recipe: Writing to standard error"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When writing Bash scripts, it is important to have a way to communicate errors or important information to the user. This is where writing to standard error comes in. By utilizing this feature, you can ensure that your script is robust and can handle unexpected errors or situations.

## How To

To write to standard error in Bash, you can use the `>&2` syntax after the `echo` command. This redirects the output of the `echo` command to standard error instead of standard output. Let's take a look at an example:

```Bash
#!/bin/bash

# This script will output a message to standard error
echo "This is a sample error message" >&2
```

If we run this script, we will see that the output is directed to standard error:

```Bash
$ ./script.sh
This is a sample error message
```

We can also use the `printf` command to write to standard error:

```Bash
#!/bin/bash

# This script will use printf to output to standard error
printf "%b\n" "This is a sample error message" >&2
```

## Deep Dive

You may be wondering why we need to use `>&2` to redirect output to standard error instead of just using `echo` or `printf` without it. The reason is that standard error is a separate output stream from standard output. This means that while standard output is typically used for regular program output, standard error is specifically designed for error messages and important information.

By writing to standard error instead of standard output, we can differentiate between regular output and error messages, allowing us to better handle and troubleshoot issues in our Bash scripts.

## See Also

Here are some additional resources and examples for writing to standard error in Bash:

- [Bash output and redirection](https://www.gnu.org/software/bash/manual/html_node/Redirections.html) by GNU
- [Linux Journal article on standard output and error](https://www.linuxjournal.com/content/working-both-sides-pipes) by Linux Journal
- [Bash error handling](https://linuxize.com/post/bash-error-handling/) by Linuxize