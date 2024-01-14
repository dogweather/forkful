---
title:                "Bash recipe: Writing tests"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Testing is an essential part of the software development process. It ensures that our code is functioning as expected and can catch any errors or bugs before they make it into production. Writing tests also helps to improve the overall quality and reliability of our code. In this blog post, we will discuss how to write tests in Bash programming.

## How To
To write tests in Bash, we will be using the `test` command. This command evaluates an expression and returns a status code of either 0 (true) or 1 (false). Let's take a look at a simple example:

```Bash
# Create a file called test.sh
touch test.sh

# Write a test for checking if a file exists
if [ -f "test.sh" ]; then
  echo "File exists!"
else
  echo "File does not exist!"
fi
```

In this example, we are using the `-f` flag to check if the `test.sh` file exists. If it does, the `if` statement will return a status code of 0, and the `echo` statement will print "File exists!". Otherwise, the status code will be 1, and the `echo` statement will print "File does not exist!".

We can also use the `!` (not) operator to negate the expression, for example:

```Bash
# Check if a file does not exist
if [ ! -f "test2.sh" ]; then
  echo "File does not exist!"
fi
```

This will return a status code of 0 if the file does not exist and print "File does not exist!".

## Deep Dive
The `test` command has many other flags that we can use to perform different checks and comparisons. For example, we can use the `-d` flag to check if a directory exists, the `-s` flag to check if a file is not empty, and the `-eq` flag to compare two numbers:

```Bash
# Check if a directory exists and is not empty
if [ -d "testdir" ] && [ -s "testdir" ]; then
  echo "Directory exists and is not empty!"
fi

# Compare two numbers
if [ $1 -eq $2 ]; then
  echo "The numbers are equal!"
fi

# Check for a substring match
if [[ "$string" == *"$substring"* ]]; then
  echo "Substring found in string!"
fi
```

It is important to note that when comparing strings, we need to wrap them in double quotes to prevent unexpected behavior.

## See Also
- [Bash test command documentation](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Introduction to Bash scripting](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Writing tests in Bash using BATS](https://dev.to/carlosrufo/writing-tests-in-bash-using-bats-488a)