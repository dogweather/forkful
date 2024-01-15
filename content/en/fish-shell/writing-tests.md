---
title:                "Writing tests"
html_title:           "Fish Shell recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests may seem like a tedious task, but it plays a crucial role in ensuring that our code is functioning correctly. It helps us catch bugs and errors early on, saving us time and trouble in the long run.

## How To
Writing tests in Fish Shell is fairly straightforward. Simply create a new file with the .fish extension and use the `test` command to validate different conditions. Here's an example:

```Fish Shell
# This test ensures that the output of `ls` is "file1 file2 file3"
test ls = "file1 file2 file3"
```

We can also use `test -n` to check if a value is not empty and `test -z` to check if it is empty. Here's an example:

```Fish Shell
# This test ensures that the variable "name" is not empty
test -n $name

# This test ensures that the variable "age" is empty
test -z $age
```

By using different flags and functions, we can create multiple tests to cover various scenarios in our code. It is also recommended to use the `set` command to define variables before running tests to ensure consistency.

## Deep Dive
When writing tests, it's important to thoroughly understand the `test` command and its different flags. For example, we can use `test -e` to check if a file or directory exists, `test -f` to check if it is a regular file, and `test -d` to check if it is a directory.

We can also use `test -nt` and `test -ot` to compare modification times of files and determine which one was changed more recently. Additionally, the `test` command supports logical operators such as `&&` and `||` for more complex tests.

Writing tests can also help us clearly define the expected outcomes of our code. This makes it easier to debug and maintain our code in the future. It also promotes a more structured and organized coding style.

## See Also
Here are some helpful resources for writing tests in Fish Shell:

- [Fish Shell Official Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial by Linuxize](https://linuxize.com/post/fish-shell/)
- [Writing Effective Tests in Fish Shell by Nick Janetakis](https://nickjanetakis.com/blog/writing-effective-tests-in-fish-shell)