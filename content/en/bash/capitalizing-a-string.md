---
title:    "Bash recipe: Capitalizing a string"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

String capitalization is a common task in programming, especially when dealing with user input or data manipulation. It allows for a consistent and uniform display of text, making it easier for users to read and understand. In this blog post, we will dive into how to capitalize a string in Bash programming.

## How To

To capitalize a string in Bash, we can use the built-in command "tr" which stands for translate. The basic syntax for this command is:

```Bash
tr '[:lower:]' '[:upper:]' <<< "input_string"
```

Let's break down this command. The "tr" command takes two arguments enclosed in single quotes - the first argument is the set of characters to be translated, and the second argument is the set of characters to translate to. In this case, we are using "[:lower:]" and "[:upper:]", which represent all lowercase and uppercase letters respectively.

The "<<<" is a redirection operator that passes the input string to the "tr" command. So, when we combine these elements, we get a command that translates all lowercase letters in the input string to uppercase.

Let's see this in action with an example:

```Bash
tr '[:lower:]' '[:upper:]' <<< "hello world"
```

The output of this command will be "HELLO WORLD".

We can also use the "tr" command to capitalize individual letters in a string. For example, if we want to capitalize only the first letter of a string, we can use the following command:

```Bash
tr '[:lower:]' '[:upper:]' <<< "${input_string^}"
```

Similarly, if we want to capitalize the first letter of every word in a string, we can use:

```Bash
tr '[:lower:]' '[:upper:]' <<< "${input_string^*}"
```

## Deep Dive

The "tr" command is not the only way to capitalize a string in Bash. We can also use the "sed" command, which stands for stream editor, to achieve the same result.

The basic syntax for this command is:

```Bash
sed 's/.*/\U&/' <<< "input_string"
```

Let's break down this command. The "sed" command uses regular expressions to match and modify text. In this case, we are using the "s" command, which stands for substitution. The first argument after "s" is the pattern to match, which in this case is ".*" meaning any character. The second argument is the replacement pattern, which is represented by "\U&" - "\U" stands for uppercase, and "&" represents the matched text. This command will substitute the entire input string with its uppercase equivalent.

We can also use "sed" to capitalize the first letter or the first letter of every word in a string, similar to the "tr" command.

## See Also

- [Bash Manual: tr Command](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#tr)
- [Bash Manual: sed Command](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#sed)
- [Regular Expressions in Bash](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions-in-sed.html)