---
title:    "Bash recipe: Checking if a directory exists"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to check if a directory exists before executing a command in a Bash script? Perhaps you wanted to check if a certain configuration file was present in a specific directory before continuing with your code. In such cases, it is important to have a way to determine if a directory exists or not in order to avoid runtime errors.

## How To

To check if a directory exists in Bash, we can use the "test" command or the conditional expression [[ -d "directory_name" ]]. This works by checking the return code of the command, which will be 0 if the directory exists and 1 if it does not.

Here is an example using the "test" command:

```Bash
test -d "/home/user/Documents"
if [ $? -eq 0 ]; then
    echo "The directory exists."
else
    echo "The directory does not exist."
fi
```

And here is the same example using the conditional expression:

```Bash
if [[ -d "/home/user/Documents" ]]; then
    echo "The directory exists."
else
    echo "The directory does not exist."
fi
```

In both cases, if the directory exists, the output will be "The directory exists." and if it does not, the output will be "The directory does not exist."

## Deep Dive

As mentioned before, the "test" command and conditional expression both check the return code to determine the existence of a directory. This return code is set by the "stat" command, which returns information about a file or directory.

When using the "test" command or conditional expression, the "-d" option is used to check if the given path is a directory. If the path is a directory, the return code will be 0 and if it is not, the return code will be 1.

It is also worth noting that these methods do not just check for the existence of the directory, but also check for the read and execute permissions of the directory. So if the directory exists but does not have the necessary permissions, the return code will still be 1.

## See Also

- [Bash test command](https://linuxize.com/post/bash-test-command/)
- [Bash conditional expressions](https://www.gnu.org/software/bash/manual/html_node/Conditional-Expressions.html)
- [Bash stat command](https://linuxize.com/post/how-to-use-linux-stat-command/)