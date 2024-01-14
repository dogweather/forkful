---
title:    "Bash recipe: Finding the length of a string"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When writing Bash scripts, it's often necessary to manipulate strings. One important piece of information when working with strings is their length. Knowing the length of a string can help with various tasks such as validation or formatting.

## How To

To find the length of a string in Bash, we can use the `expr` command along with the `length` function. Here's an example of how to use it:

```Bash
my_string="Hello, world!"
string_length=$(expr length $my_string)
echo "The length of the string is $string_length" 
```

The output of this code would be:

```
The length of the string is 13
```

We first declare our string, "Hello, world!", in the `my_string` variable. Then, we use the `expr` command, which allows us to evaluate expressions. Inside the parentheses, we use the `length` function and pass in the variable containing our string.

We assign the output of this expression to the `string_length` variable, using the `$( )` syntax to capture the output and save it to a variable.

Finally, we can print out the length of our string by using `echo` and referencing the `string_length` variable.

## Deep Dive

While using `expr length` is the most common way to find the length of a string in Bash, there are a few other methods. One alternative is to use the `wc` (word count) command with the `-m` option, which stands for "character count". Here's an example:

```Bash
my_string="Hello, world!"
string_length=$(echo -n $my_string | wc -m)
echo "The length of the string is $string_length"
```

The output would be the same as before, 13. However, this method also takes into account any spaces or special characters in the string, which may be useful depending on your needs.

Another way to find the length of a string is to use the `${#variable}` syntax. This will return the length of the variable without having to use the `expr` command. Here's an example:

```Bash
my_string="Hello, world!"
string_length=${#my_string}
echo "The length of the string is $string_length"
```

Again, the output would be 13.

## See Also

- Bash `expr` command documentation: https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html
- Bash `wc` command documentation: https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html
- Bash string manipulation: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion