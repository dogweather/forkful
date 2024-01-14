---
title:    "Bash recipe: Writing to standard error"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why 

When writing scripts in Bash, it is important to properly manage errors and display relevant information to the user. This is where writing to standard error comes in. By sending error messages to standard error, instead of standard output, it allows for more efficient error handling and helps to differentiate between regular output and errors. 

## How To 

To write to standard error in Bash, you can simply use the `>&2` notation after your command. This redirects the output of the command to standard error instead of standard output. For example: 

```Bash
ls -l /fake/directory >&2
```

This will display the error message "ls: cannot access '/fake/directory': No such file or directory" to standard error instead of the usual output of the `ls` command. 

You can also use the `echo` command to directly print messages to standard error. For example: 

```Bash
echo "Error: Invalid input" >&2
```

This will print the error message "Error: Invalid input" to standard error. 

## Deep Dive

Standard error, also known as `stderr`, is a type of output stream that is used to display error messages and diagnostic information in Bash. It is typically used by programs to communicate any errors or issues encountered during execution. By default, stderr is set up to display error messages to the user, while regular output is displayed through standard output (`stdout`). 

In Bash, both standard error and standard output can be redirected to different places using the `>&` notation. You can redirect stderr to a file or ignore it altogether by using `2>/dev/null`. 

It is important to note that not all errors are sent to standard error. Some programs may send errors to `stdout` instead. To solve this, you can use pipes to separate regular output from error messages. For example: 

```Bash
ls -l /fake/directory 2>&1 | grep "ls: cannot access"
```

This will redirect the error message to standard output, which will then be piped to `grep` to search for the specific error message. 

## See Also 

- [Bash Guide for Beginners: Redirections](https://linuxconfig.org/bash-guide-for-beginners-redirections)
- [Bash Hackers Wiki: Standard error](https://wiki.bash-hackers.org/howto/redirection_tutorial#standard_error)
- [Linux Journal: Standard Output and Standard Error](https://www.linuxjournal.com/content/standard-output-and-standard-error)