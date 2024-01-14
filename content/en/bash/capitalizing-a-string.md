---
title:    "Bash recipe: Capitalizing a string"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String in Bash

Capitalizing a string in Bash can be useful when working with user inputs or manipulating textual data. It can make the output more visually appealing or conform to a certain format.

## How To Capitalize a String in Bash

To capitalize a string in Bash, we can use the `awk` command. The following code snippet shows how to capitalize the first letter of a string:

```
#!/bin/bash

# Define the string to capitalize
str="hello world"

# Use awk to capitalize the first letter
capitalized_str=$(echo "$str" | awk '{print toupper(substr($0, 1, 1)) substr($0, 2)}')

# Print the capitalized string
echo "$capitalized_str"

# Output: Hello world
```

We can also use the `tr` command to capitalize the entire string:

```
#!/bin/bash

# Define the string to capitalize
str="hello world"

# Use tr to capitalize the entire string
capitalized_str=$(echo "$str" | tr '[:lower:]' '[:upper:]')

# Print the capitalized string
echo "$capitalized_str"

# Output: HELLO WORLD
```

Another way to capitalize a string is by using Bash's string manipulation capabilities. The following code snippet shows how to capitalize the first letter using parameter expansion:

```
#!/bin/bash

# Define the string to capitalize
str="hello world"

# Use parameter expansion to capitalize the first letter
capitalized_str="${str^}"

# Print the capitalized string
echo "$capitalized_str"

# Output: Hello world
```

## Deep Dive into Capitalizing a String in Bash

In Bash, strings are treated as arrays of characters. We can use this characteristic to manipulate the string as needed. For example, to capitalize the first letter of a string, we can use the `^` operator, which capitalizes the first character.

Similarly, we can use the `^^` operator to capitalize every character in the string, or the `,,` operator to convert the string to lowercase.

Additionally, we can use the `tr` command with its various options to manipulate the case of the string. For example, we can use `tr '[:lower:]' '[:upper:]'` to convert all lowercase characters to uppercase.

Overall, understanding the characteristics and capabilities of strings in Bash can help in efficiently capitalizing strings in a variety of ways.

## See Also

- [Bash Guide: String Manipulation](https://www.linuxjournal.com/article/8919)
- [AWK Tutorial](https://www.grymoire.com/Unix/Awk.html)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)