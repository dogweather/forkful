---
title:                "Bash recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Bash is a popular command-line language that allows users to automate tasks through programming. One useful task that Bash can handle is extracting substrings from strings. This can be helpful for text manipulation, data processing, and more.

## How To

To extract substrings in Bash, we use the built-in `cut` command. It allows us to specify a delimiter to split the string and then select the desired substring. Let's walk through an example.

Suppose we have a string `Hello World!`, and we want to extract the word `World`. Using `cut`, we can specify the space as our delimiter and choose the second field, which corresponds to the word `World`.

```Bash
string="Hello World!"
substring=$(echo $string | cut -d " " -f 2)
echo $substring
```

The output of this code will be `World`, which is the substring we wanted. It is essentially splitting the string at the space and selecting the second item.

We can also use the same method to extract multiple substrings at once. Suppose we have a string `I love coding in Bash`, and we want to extract the words `love` and `Bash`. We can do so by specifying a space as the delimiter and selecting the second and fifth fields, respectively.

```Bash
string="I love coding in Bash"
substring1=$(echo $string | cut -d " " -f 2)
substring2=$(echo $string | cut -d " " -f 5)
echo $substring1
echo $substring2
```

The output of this code will be `love` and `Bash`, each on a separate line.

## Deep Dive

The `cut` command offers various options to specify delimiters, select fields, and more. For example, we can use `-c` to select a specific character or `-s` to remove lines that do not contain the delimiter.

Additionally, we can use the `grep` command in combination with `cut` to extract substrings that match a specific pattern. For instance, if we want to extract all words that start with the letter `B` in the string `I love Bash scripting`, we can use the following code:

```Bash
string="I love Bash scripting"
substring=$(echo $string | grep -o "\bB[a-zA-Z]*")
echo $substring
```

The output of this code will be `Bash`, `Bash`, and `Bash`, as it matches the pattern for all three words starting with `B`.

## See Also

- [Bash cut command](https://www.geeksforgeeks.org/cut-command-linux-examples/)
- [Bash grep command](https://www.geeksforgeeks.org/grep-command-in-unixlinux/)
- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)