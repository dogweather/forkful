---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
这是关于如何将字符串转化为小写。程序员经常需要将字符串转化为小写以实现程序逻辑，例如，比较用户输入、文件名等。

## 如何做：
在Fish Shell中，我们可以用`string lower`命令来轻松转化一个字符串为小写。看下面的示例：

``` fish
> set my_str "HELLO, WORLD!"
> echo $my_str | string lower
hello, world!
```
非常直接，不是吗？Fish Shell在这方面提供了很棒的内置支持。

## 深入探讨
这项技巧的历史可以追溯到计算机编程的早期阶段，当时人们还在编写处理文本的基本程序。在一些其他壳中，如BASH，我们可能需要用到外部命令，例如`tr`，来实现此目的:

```bash
echo "HELLO, WORLD!" | tr '[:upper:]' '[:lower:]'
hello, world!
```
然而，Fish Shell的开发者们设计了内置的`string`命令来处理字符串，其中包括我们讨论的这个`string lower`功能。这个功能实现上采用了Unicode的正确处理方式，因此在处理有一些需要特殊处理的字符如特殊符号或者来自不同语言的字母时也不会出错。

## 另请参见
- Fish Shell文档的[字符串操作部分](https://fishshell.com/docs/current/cmds/string.html): 这里提供了Fish Shell内置的`string`命令的详细说明，包括更多的用例和其他有用的字符串操作。