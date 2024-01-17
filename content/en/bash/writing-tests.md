---
title:                "Writing tests"
html_title:           "Bash recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests is the process of creating automated code to check the correctness and functionality of your own code. It's like a safety net for your code, catching any mistakes or bugs before they make it to your end users. By writing tests, programmers can ensure that their code is efficient, reliable, and performs as intended.

## How to:
Writing tests in Bash is a straightforward process. The `test` command, which is also known as `[`, evaluates a given expression and returns a true or false value. Here's an example of a simple test in Bash:

```Bash
[ 5 -gt 3 ]
```

The above command will return true, since 5 is indeed greater than 3. You can also use the `test` command in conjunction with conditional statements, such as `if` and `while` to create more complex tests. See the example below:

```Bash
if [ -e file.txt ]  # checks if file.txt exists
then
  echo "File exists!"
else
  echo "File does not exist"
fi
```

Running this script will output "File exists!" if the file.txt exists in the current directory. Otherwise, it will output "File does not exist."

## Deep Dive:
The `test` command has been a part of the Bash language since its early days. It was created by Stephen Bourne in the 1970s and was included in the first version of Unix. Another popular alternative for writing tests in Bash is the `[[` command, which was introduced in Bash 2.0 and offers additional features such as pattern matching and regular expressions. However, the `test` command is still widely used and is more portable across different platforms.

When writing tests in Bash, it's important to consider the exit status of the `test` command. A successful test will return an exit status of 0, while a failed test will return a non-zero exit status. This is important when using tests in conjunction with other commands or in scripts. You can view the exit status of the previous command by typing `echo $?` in your terminal.

## See Also:
- [Bash Test Command](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Bash conditional statements](https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html)