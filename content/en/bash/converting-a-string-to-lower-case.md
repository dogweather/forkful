---
title:    "Bash recipe: Converting a string to lower case"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to lower case in your Bash program? Whether you're working with user input or manipulating file names, converting to lower case can be a useful task to ensure consistency in your code. In this blog post, we'll explore how to convert a string to lower case in Bash programming.

## How To

First, let's take a look at a simple example using the `tr` command. This command is used for translating or deleting characters in a string. In this case, we will use it to convert the string to lower case.

```
Bash -c 'echo "HELLO WORLD!" | tr "[:upper:]" "[:lower:]"'
```

The output of this command will be `hello world!`. As you can see, the string has been converted to lower case.

Another option is to use the built-in `awk` command. This command allows you to process text and extract data in a structured manner. We can use the `tolower` function in `awk` to convert our string to lower case.

```
Bash -c 'echo "Hello World!" | awk '{print tolower($0)}''
```

This will also result in `hello world!` as the output. 

Lastly, we can use the `sed` command to replace specific patterns in the string. This method is useful if you only want to convert certain characters to lower case.

```
Bash -c 'echo "HeLLo+= WorLd!" | sed 's/[A-Z]/\L&/g''
```

The output will be `hello+= world!`, with only the letters converted to lower case while the special characters remain the same.

## Deep Dive

Now that we've seen some examples of converting a string to lower case, let's take a deeper dive into how it works. In the first example using `tr`, we specify the `[:upper:]` and `[:lower:]` classes inside the double quotes. These classes represent all uppercase and lowercase letters, respectively. The `tr` command essentially translates every uppercase letter to its lowercase counterpart.

Next, in the `awk` command, we use the `tolower` function to convert the string to lowercase. This function reads the provided string and returns the lowercase version of it.

In the `sed` command, we use the `s` command to search for any uppercase letters `[A-Z]` in the string and replace them with their lowercase equivalent `\L&`. The `g` at the end of the command makes sure this pattern is applied globally to the entire string.

There are also other methods for converting a string to lower case such as using the `bash` built-in `${parameter,,pattern}` expansion, or using the `printf` command with the `%Ls` format specifier.

## See Also

- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Bash Pitfalls](https://mywiki.wooledge.org/BashPitfalls)
- [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)