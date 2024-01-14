---
title:    "Bash recipe: Extracting substrings"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Bash programming is a powerful tool for automating tasks and extracting useful information from text. One useful feature of Bash is the ability to extract substrings from a given string. This can be particularly useful in manipulating data or formatting text in your scripts.

## How To

To extract substrings in Bash, we can use the `cut` command. This command allows us to specify a delimiter and extract the desired substring from a given string. Let's look at an example:

```
#!/bin/bash

# Define a string with the desired substring
str="Welcome to my blog post!"

# Use cut to extract the substring "blog"
blog=$(echo $str | cut -d ' ' -f 4)

# Print the extracted substring
echo $blog
```

In the code above, we first define a string containing the desired substring "blog". Then, we use the `cut` command to specify the delimiter as a space and extract the fourth field, which is "blog". Finally, we print the extracted substring. The output of this script will be `blog`, confirming that the substring was successfully extracted.

We can also specify a range of characters to extract instead of a specific field. For example, if we want to extract the first five characters of a string, we can use the following command:

```
echo "Hello World" | cut -c 1-5
```

This will output `Hello`, which is the first five characters of the string.

## Deep Dive

Behind the scenes, the `cut` command uses `awk` to extract the desired substring. Awk is a programming language designed for text processing, making it a powerful tool for manipulating strings in Bash.

Additionally, we can use regular expressions with the `cut` command to specify more complex delimiters. This allows us to extract substrings based on patterns instead of just specific characters.

It's worth noting that `cut` is not the only way to extract substrings in Bash. We can also use the `sed` command or Bash's built-in pattern matching operators to achieve similar results.

## See Also

Here are some useful resources for further reading on extracting substrings and text processing in Bash:

- [Bash Guide](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Regular Expressions in Bash](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)
- [Awk Tutorial](https://www.grymoire.com/Unix/Awk.html)
- [Sed Tutorial](https://www.grymoire.com/Unix/Sed.html)
- [Linux Command Library](https://www.linuxcommand.org/)