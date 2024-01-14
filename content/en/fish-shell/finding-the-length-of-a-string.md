---
title:    "Fish Shell recipe: Finding the length of a string"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string is a fundamental task in programming. Whether you are working with user input, manipulating text, or validating data, being able to accurately determine the length of a string is a valuable skill to have. In this blog post, we will explore how to use Fish Shell to easily find the length of a string and why it is important in programming.

## How To

Coding Example:

```Fish Shell

set string "Hello world"
set length (echo -n $string | wc -m)
echo $length
```

Sample Output:

```
12
```

In this example, we first set a variable "string" to the value of "Hello world". Then, using the "wc" command with the "-m" flag, we are able to count the number of characters in the string and store it in a new variable "length". Finally, we echo the value of "length" to see the output, which is the total number of characters in the string.

Fish Shell makes finding the length of a string simple and efficient. By using the "echo" and "wc" commands, we are able to accurately count the number of characters in a string without having to manually count them ourselves. This allows for more precise and error-free coding.

## Deep Dive

There are a few things to keep in mind when using the "wc" command to find the length of a string in Fish Shell. Firstly, the "-m" flag is necessary to ensure that the count includes all characters, including spaces and special characters. Without this flag, the count may be inaccurate.

Additionally, the "echo -n" command is used to prevent the line break from being counted as an extra character. This is important because adding a new line character can often change the results of the count.

It is also worth noting that in Fish Shell, the "wc" command is included in the coreutils package. This means that it is readily available for use without needing to install any additional packages.

## See Also

For more information on using the "wc" command in Fish Shell, you can refer to the official Fish Shell documentation here: https://fishshell.com/docs/current/cmds/wc.html.

You may also be interested in learning more about other string manipulation techniques in Fish Shell, such as splitting and trimming strings. You can find more information and examples in the official documentation here: https://fishshell.com/docs/current/index.html.

Happy coding!