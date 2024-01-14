---
title:                "Bash recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Why Writing to Standard Error is Useful

When writing a Bash script, it is important to be able to handle errors efficiently. This is where writing to standard error comes in. By outputting error messages to standard error, you can easily differentiate them from regular output and handle them appropriately.

##How To Write to Standard Error in Bash

Writing to standard error in Bash is simple. You just need to use the `1>&2` redirection. For example:

```Bash
echo "This is regular output"
echo "This is an error" 1>&2
```

The `1>&2` redirects the output of the second `echo` command to standard error instead of standard output. This allows the error message to be displayed separately from the regular output.

To see this in action, let's run the following script:

```Bash
#!/bin/bash

echo "Starting execution..."

ls non-existent-dir 1>&2

echo "Execution complete."
```

The output will be:

```
Starting execution...
ls: cannot access 'non-existent-dir': No such file or directory
Execution complete.
```

As you can see, the error message from the `ls` command is displayed separately from the regular output.

##Deep Dive into Writing to Standard Error

In Bash, standard error is represented by file descriptor 2. When using the `1>&2` redirection, we are essentially redirecting the output of a command to file descriptor 2, which is standard error. This allows us to handle error messages separately from regular output.

It is important to note that standard error is not affected by the `2>&1` redirection. This means that even if you redirect standard output to standard error, error messages will still be displayed separately. For example:

```Bash
command 2>&1  # redirects standard output to standard error, but error messages will still be displayed separately
```

Additionally, you can also redirect standard error to a file using `2>filename` or append to a file using `2>>filename`.

##See Also

- [Understanding File Descriptors in Bash](https://linuxize.com/post/bash-file-descriptors/)
- [Redirections in Bash](https://www.tldp.org/LDP/abs/html/io-redirection.html)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)

Writing to standard error is a useful technique to handle errors in Bash scripts. By using the `1>&2` redirection, you can easily handle error messages separately from regular output. For more advanced use cases, you can also redirect standard error to a file or append to an existing file. The key to writing efficient Bash scripts is understanding file descriptors and how to manipulate them to your advantage.