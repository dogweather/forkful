---
title:                "Bash recipe: Capitalizing a string"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Why
Are you tired of seeing strings in all lowercase letters? Do you want to add some emphasis to your strings? Look no further! Capitalizing a string can easily be done in Bash programming.

##How To
Coding in Bash may seem intimidating at first, but it's actually quite simple. To capitalize a string, follow these steps:

1. Start by creating a variable that contains the string you want to capitalize. For this example, we'll use the string "hello world".
```
Bash
string="hello world"
```
2. Next, we can use the `tr` command to translate lowercase characters to uppercase. The `-u` flag specifies that we want to convert to uppercase.
```
Bash
capitalized_string=`echo $string | tr '[:lower:]' '[:upper:]'`
```
3. Finally, we can print out the capitalized string using the `echo` command.
```
Bash
echo $capitalized_string
```
The output of this code will be:
```
HELLO WORLD
```
Congratulations, you have successfully capitalized a string in Bash!

##Deep Dive
Behind the scenes, the `tr` command works by using a translation table. In our example, the translation table has all lowercase letters mapped to their uppercase counterparts. This means that when we use the `tr` command, the lowercase "h" in "hello world" is translated to an uppercase "H", and so on.

Additionally, it's worth noting that the `tr` command only works with single characters. If you try to pass in a string, it will only process the first character. This is why we had to use the `echo` command to pass in the entire string.

##See Also
To dive even deeper into the `tr` command, check out these resources:
- [The tr command in Bash](https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/)
- [Using Bash's tr command](https://linuxize.com/post/linux-tr-command/)
- [Utilities: tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)