---
title:                "Concatenating strings"
html_title:           "Bash recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a useful skill to have in Bash programming as it allows you to combine multiple strings together to create a new string. This can be helpful for creating dynamic output or manipulating data.

## How To

To concatenate strings in Bash, you can use the `+` operator or the `+=` compound assignment operator. Here's an example:

```Bash
first_name="John"
last_name="Doe"

full_name=$first_name" "$last_name

#or using compound assignment:
full_name+=" Jr."

echo $full_name
#output: John Doe Jr.
```

In the above example, we have two variables `first_name` and `last_name` which contain the first and last name respectively. We used the `+` operator to add a space in between the two names and store it in a new variable `full_name`. We then used the `+=` operator to add the suffix "Jr." to the end of the `full_name` variable. Finally, we printed out the `full_name` variable to see the concatenated string.

You can also use the `cat` command to concatenate strings. Here's an example:

```Bash
str1="Hello"
str2="world!"

echo `cat <<< "$str1 $str2"`
#output: Hello world!
```

Here, we used the `<<<` input redirection operator to pass the two strings into the `cat` command, which then concatenates them and prints out the result.

## Deep Dive

There are a few things to keep in mind when concatenating strings in Bash:

- Double quotes should be used when assigning concatenated strings to a variable to prevent word splitting and globbing. For example, `full_name=$first_name" "$last_name` is better than `full_name=$first_name $last_name`.
- Arrays can also be concatenated using the `+=` operator. For example, `nums+=(10 20 30)` will add the numbers 10, 20 and 30 to the end of the `nums` array.
- You can also use command substitution to concatenate strings from the output of a command. For example, `pathname=$(pwd)" documents"` will add the string " documents" to the end of the current working directory.

## See Also

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) - helpful resource for learning more about Bash scripting.
- [Introduction to Bash Scripting](https://www.pluralsight.com/guides/introduction-to-bash-scripting) - another article on Bash scripting for beginners.
- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/html_node/index.html) - official documentation for Bash.