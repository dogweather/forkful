---
title:                "Bash recipe: Converting a string to lower case"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to lower case in your Bash script? Maybe you want to make sure that the user's input is in a consistent format, or you need to compare two strings but case-sensitivity is not relevant. No matter the reason, knowing how to convert a string to lower case in Bash can be a useful skill to have in your programming arsenal.

## How To

To convert a string to lower case in Bash, we can use the `tr` command which is used for translating or deleting characters. Specifically, we will be using the `-s` option of `tr` which removes duplicate characters and the `-d` option which deletes specified characters.

```Bash
// Example code block showing a variable assignment and the tr command with -s and -d options
string="HELLO WORLD"
lowercase=$(echo $string | tr -s [:upper:] [:lower:] | tr -d ' ')
```

In the above code, we first assign the string "HELLO WORLD" to a variable named `string`. Then, we use the `echo` command to print the value of `string` and pass it as input to the `tr` command. The `-s` option takes two character sets as arguments. In this case, we specify the `[:upper:]` character set to represent all uppercase letters and the `[:lower:]` character set to represent all lowercase letters. This will convert all uppercase letters in the string to lowercase. Next, we use the `-d` option with the `tr` command to specify which characters to delete. In this example, we delete the space character (represented by the single quotes) to remove any whitespace in the converted string. Finally, we assign the output of `tr` to a new variable named `lowercase` which will contain the lowercase version of the string "Hello World".

The output of this code will be as follows:

```Bash
echo $lowercase
hello world
```

We can also use the `tr` command on a variable without using the `echo` command. For example:

```Bash
// Example code block showing the use of `tr` command on a variable directly
string="HELLO WORLD"
lowercase=$(tr -s [:upper:] [:lower:] <<< $string)
```

The output of this code will be the same as the previous example.

## Deep Dive

We can also use other methods to convert a string to lower case in Bash, such as using the `sed` command or using `bash` built-in parameter expansion.

```Bash
// Example code block showing the use of `sed` command
string="HELLO WORLD"
lowercase=$(echo $string | sed 's/./\L&/g')
```

In this example, we use `sed` to substitute each character in the string with its lowercase version.

```Bash
// Example code block showing the use of `bash` built-in parameter expansion
string="HELLO WORLD"
lowercase=${string,,}
```

In this example, we use `bash` built-in parameter expansion to convert the value of the variable `string` to lowercase.

However, using the `tr` command is the most common and efficient way to convert a string to lower case in Bash.

## See Also

Here are some useful resources for further reading on converting strings to lower case in Bash:

- [Bash String Manipulation](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash String Operators](https://www.gnu.org/software/bash/manual/html_node/String-Operators.html)
- [tr command in Linux with examples](https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/)