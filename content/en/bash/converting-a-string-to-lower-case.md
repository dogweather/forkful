---
title:                "Bash recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

When working with strings in Bash programming, it's important to have the ability to manipulate them in different ways. Converting a string to lower case is a common task that can be useful in various scenarios. 

## How To

To convert a string to lower case in Bash, we can use the built-in `tr` command. This command allows us to perform text transformations on standard input or a given file. To convert a string to lower case, we can use the `-a` flag which specifies that all characters should be converted to lower case. Let's take a look at an example.

```
#!/usr/bin/env bash

#defining a string variable
string="HELLO WORLD"

#converting the string to lower case using 'tr'
lowercase=$(echo "$string" | tr -a '[:upper:]' '[:lower:]')

#outputting the result
echo "$lowercase"
```

The output of this script would be:
```
hello world
```

Here, we have used the `echo` command to print the result and saved it in a variable called `lowercase`. We have also used the `|` symbol to pipe the output of `echo` as the input for the `tr` command. This allows us to convert the string to lower case without changing the value of the original variable.

We can also use the same approach to convert the contents of a file to lower case. Let's take a look at another example.

```
#!/usr/bin/env bash

#defining the file path
file="example.txt"

#converting the contents of the file to lower case and saving it in a new file
echo "$(cat "$file")" | tr -a '[:upper:]' '[:lower:]' > lowercase.txt

#outputting the result
echo "Successfully converted '$file' to lower case and saved it in 'lowercase.txt'"
```

The output of this script would be:
```
Successfully converted 'example.txt' to lower case and saved it in 'lowercase.txt'
```

Here, we have used the `cat` command to print the contents of the file and used `>` to redirect the output to a new file called `lowercase.txt`. This allows us to preserve the original file while creating a new file with the lower case version of the contents.

## Deep Dive

Under the hood, the `tr` command uses ASCII codes to convert the characters in the string to lower case. It takes two sets of characters as arguments - the first set represents the characters to be replaced, and the second set represents the characters to be used for replacement. In our examples, we have used `[:upper:]` and `[:lower:]` to specify all upper case and lower case characters respectively. However, we can also use a range of ASCII codes for more specific conversions. For example, if we only want to convert letters from A to D to lower case, we can use `A-D` as the first set and `a-d` as the second set. 

## See Also

- [Bash tr command](https://linux.die.net/man/1/tr)
- [Character encoding](https://www.w3schools.com/charsets/)
- [ASCII table](https://www.asciitable.com/)