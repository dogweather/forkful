---
title:                "Bash recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a small task, but it can actually make a big difference in the overall appearance and readability of your code. By capitalizing certain words or phrases, you can add emphasis or make them stand out more, making your code easier to understand and follow.

## How To

Capitalizing a string in Bash is a simple process. Follow these steps to learn how to do it:

1. First, you will need to open your terminal and navigate to the directory where you want to create your Bash script.
2. Inside this directory, create a new file and give it a name, such as "capitalize.sh".
3. Within this file, start by declaring a variable and assigning it a string value that you want to capitalize. For example, `str="hello, world"`.
4. Next, use the `tr` command to make the string uppercase. The syntax for this would be `tr '[:lower:]' '[:upper:]' <<< $str`. This command uses the `tr` utility to convert all lowercase letters in the string to uppercase.
5. Lastly, use the `echo` command to print the capitalized string to the terminal. The full code should look like this:

```Bash
#!/bin/bash
str="hello, world"
echo $(tr '[:lower:]' '[:upper:]' <<< $str)
```
6. Save and exit the file, then make it executable by using the command `chmod +x capitalize.sh`.
7. Finally, run the script by using `./capitalize.sh` and you should see the output as `HELLO, WORLD`.

## Deep Dive

While the `tr` command is a quick and easy way to capitalize a string, there are other methods you can use as well. For example, you can use the `awk` command with the `toupper` function or the `sed` command with the `s//` substitution. Both of these methods also allow you to specify which characters you want to capitalize, giving you more control over the output.

It's important to keep in mind that the `tr` command and other methods may have different results depending on the locale and language settings in your terminal. So if you're not seeing the expected results, try setting your locale to "C" with the command `export LC_ALL=C` before running the script again.

## See Also

If you want to learn more about string manipulation in Bash, check out these resources:

- [How To Work with Strings in Bash](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-bash)
- [The Linux Documentation Project - Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/Bash-Beginners-Guide.pdf)
- [BashGuide - Manipulating Variables](https://mywiki.wooledge.org/BashGuide/ManipulatingVariables)