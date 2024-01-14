---
title:    "Bash recipe: Finding the length of a string"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Many programming languages have a built-in function to find the length of a string, but did you know that Bash also has a way to do this? If you're new to Bash scripting, knowing how to find the length of a string can come in handy for tasks such as validating user input or manipulating string data. In this blog post, we'll explore the different methods to find the length of a string in Bash.

## How To

To find the length of a string in Bash, we can use the `expr` command or the `wc` command. Let's look at some examples:

**Using `expr` command:**

```Bash
# Define a string variable
my_string="Hello World"

# Use expr to find the length of the string
expr length "$my_string"

# Output: 11 (the length of the string, including spaces)
```

**Using `wc` command:**

```Bash
# Define a string variable
my_string="Hello World"

# Use wc to find the number of characters in the string
echo -n "$my_string" | wc -m

# Output: 11 (the number of characters in the string, excluding spaces)
```

You may be wondering why we used `echo -n` in the second example. This is because the `wc` command counts the new line character at the end of each line, so we need to use `echo -n` to suppress the new line character.

## Deep Dive

Now, let's delve deeper into how these commands work. The `expr` command takes an expression and evaluates it. In our case, we are using the `length` argument with the `$my_string` variable, which returns the length of the string.

The `wc` command, on the other hand, stands for "word count" and is commonly used to count the number of lines, words, and characters in a file. In our example, we are using the `-m` flag to count the number of characters. You can explore the other flags of the `wc` command to see how they work.

Another way to find the length of a string in Bash is by using Parameter Expansion. This is a feature of Bash that allows for manipulating variables. Here's an example of how we can use Parameter Expansion to find the length of a string:

```Bash
# Define a string variable
my_string="Hello World"

# Use Parameter Expansion to find the length of the string
echo "${#my_string}"

# Output: 11 (the length of the string)
```

## See Also

- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Bash expr command](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Bash wc command](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)