---
title:    "Bash recipe: Converting a string to lower case"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why 
If you're new to Bash programming, you may have come across the need to convert a string to lower case. This can be useful when dealing with user input or manipulating text in your scripts. Converting to lower case allows for easier comparison and manipulation of strings. 

## How To
To convert a string to lower case in Bash, you can use the built-in command `tr`. The syntax for `tr` is as follows: 

```
tr [options] SET1 SET2
```

To convert a string to lower case, we need to specify the range of characters to be converted in `SET1` and the corresponding characters in `SET2`. In this case, we want to convert all upper case letters to lower case, while leaving the rest of the string unchanged. We can do this by specifying the range of upper case letters in `SET1` and the matching lower case letters in `SET2`. Here's an example:

```
sentence="Hello World!"
lowercase=$(echo $sentence | tr '[A-Z]' '[a-z]')
echo $lowercase
```

The output for this will be `hello world!`, with all letters converted to lower case. 

## Deep Dive
In Bash, characters are represented by their ASCII codes. The uppercase letters A-Z have ASCII codes ranging from 65 to 90, while the lowercase letters a-z have ASCII codes ranging from 97 to 122. This is why we specify these ranges in `SET1` and `SET2` when using `tr` to convert strings to lower case. 

It's also worth noting that the `tr` command is not just limited to converting case. It can be used for a wide range of text manipulations, such as replacing characters, removing duplicates, and translating characters from one set to another. 

## See Also
- [Bash `tr` command](https://linux.die.net/man/1/tr)
- [ASCII Table](https://www.asciitable.com/)