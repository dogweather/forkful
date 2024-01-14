---
title:    "Fish Shell recipe: Writing tests"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Why: The Importance of Writing Tests

As programmers, we know that testing our code is crucial for ensuring its quality and functionality. But why should we specifically focus on writing tests in Fish Shell? 

Writing tests in Fish Shell allows us to easily automate the process of testing our code, saving us time and effort in the long run. Additionally, tests help to catch bugs and errors early on, making for a more efficient and streamlined debugging process. It also serves as a form of documentation for our code, providing a clear understanding of its purposes and expected outputs.

## How To: Writing Tests in Fish Shell

Writing tests in Fish Shell is a simple and effective way to ensure the success and accuracy of our code. Let's take a look at a basic example:

```
Fish Shell test -n "Hello World" -o echo "Hello World"
```

In the above code, we are using the "test" command in Fish Shell to check if the string "Hello World" is outputted by the "echo" command. If the test is successful, "Hello World" will be printed in the terminal. 

We can also use the "set -q" command to verify if a certain variable is set. For example:

```
set -q FISH
and set -q FISH; or echo "Fish Shell is awesome!"
```

This code checks if the variable "FISH" is set, and if it is, it will print "Fish Shell is awesome!" in the terminal. If it is not set, nothing will be printed.

## Deep Dive: Understanding the Fish Shell Test Command

The "test", or " [" command in Fish Shell is used for evaluating conditions and returning a boolean value. It takes in the condition to evaluate, and based on its result, will return either a "true" or "false" value.

In Fish Shell, we can also combine multiple tests using logical operators. For example, using the "and" operator will only return "true" if both conditions are met, while the "or" operator will return "true" if at least one of the conditions is met.

Other commonly used operators in the "test" command include:

- "-n" for checking if a string is not empty
- "-z" for checking if a string is empty
- "-e" for checking if a file or directory exists 

For a full list of operators and their functions, check out the official Fish Shell documentation.

## See Also

- [Fish Shell Documentation on Testing](https://fishshell.com/docs/current/commands.html#test)
- [Introduction to Fish Shell Testing - FreeCodeCamp](https://www.freecodecamp.org/news/introduction-to-fish-shell-testing/)
- [Writing Unit Tests in Fish Shell - Dev.to](https://dev.to/gokuldroid/writing-unit-tests-in-fish-shell-4h8c)

Writing tests in Fish Shell may seem daunting at first, but with practice and a good understanding of its syntax and commands, it can greatly improve the quality and reliability of our code. Happy testing!