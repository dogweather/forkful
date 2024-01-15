---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a fundamental skill in Bash programming that allows you to combine multiple strings into one. It is a useful technique for building complex commands and creating customized outputs.

## How To

To concatenate strings in Bash, you can use the `echo` command followed by the strings inside double quotes, separated by a space. For example:

```Bash
echo "Hello" "World"
``` 

The output will be "Hello World" with a space in between the two strings. You can also use variables to concatenate strings. For example:

```Bash
name="John"
echo "My name is" $name
```

The output will be "My name is John" with a space in between. Another way to concatenate strings is by using the `+=` operator. For example:

```Bash
title="Bash"
title+=" Programming"
echo $title
```

The output will be "Bash Programming" where the second string is appended to the first.

## Deep Dive

When concatenating strings, it is important to understand the difference between single and double quotes. Single quotes will preserve the literal value of all characters within the quotes while double quotes will allow for variable expansion and interpret certain characters, such as backslashes. This means that when using single quotes, the variables will not be evaluated and the literal text will be printed. For example:

```Bash
name="John"
echo 'My name is' $name
```

The output will be "My name is $name" with no variable evaluation.

Additionally, if you want to concatenate a string with a number, you can use the `printf` command. For example:

```Bash
num=5
printf "The number is %d" $num
```

The output will be "The number is 5" where the `%d` is replaced by the value of the variable `num`.

## See Also

- [Bash documentation](https://www.gnu.org/software/bash/manual/)
- [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [String concatenation in Bash](https://www.baeldung.com/linux/bash-string-concatenation)