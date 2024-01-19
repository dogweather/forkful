---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
字符串连接是将两个或更多字符串连接成一个更长字符串的过程。程序员这么做是为了生成动态文本，或者在输出中组合多个单独的字符串。

## 如何:
在Fish Shell中，你可以使用以下两种主要方法来连接字符串：

```
# 方法一：使用 string join 命令
> set a "Hello"
> set b "World"
> string join ' ' $a $b
Hello World
```
此方法将"$a"和"$b"连接起来，并在它们之间添加一个空格。

```
# 方法二：从Shell脚本中直接输出字符串
> set a "Hello"
> set b "World"
> echo $a$b
HelloWorld
```
此方法直接将"$a"和"$b"连接起来，不使用分隔符。

## 深入了解：
Fish Shell的设计目标之一是简单易用。早在20世纪70年代的Unix Shell就有字符串连接的功能，Fish Shell的实现方式更符合人类的认知。
Fish Shell提供了多种连接字符串的方式，如上面提到的`string join`和`echo`命令，你可以根据具体场景选择使用。
字符串连接在各种程式语言中都有不同实现方式。例如，在Python中，可以使用`+`运算符连接字符串，PHP则使用`.`。

## 另请参阅：
- Fish Shell的[官方手册](https://fishshell.com/docs/current/index.html)对该主题有深入的研究。
- 对于字符串操作的各种情况，[GNU操作手册](https://www.gnu.org/software/coreutils/manual/html_node/string-operations.html)是一个不错的资源。