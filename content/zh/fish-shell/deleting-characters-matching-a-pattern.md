---
title:                "删除匹配模式的字符。"
html_title:           "Fish Shell: 删除匹配模式的字符。"
simple_title:         "删除匹配模式的字符。"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中，有时候我们需要删除特定模式的字符。这可以帮助我们更轻松地处理字符串数据，并提高我们的代码效率。所以，我们可以把它看作是一种工具，帮助我们做一些重复的事情。

## 怎么做？
要在Fish Shell中删除匹配特定模式的字符，我们可以使用内置的`string`命令。例如，我们有一个字符串`Hello World`，我们想要删除所有的小写字母，可以这样写：
```
set str Hello World
string replace -r $str '[a-z]' ''
```
这将把`Hello World`变成`H W`。我们也可以使用`string remove`命令来删除匹配模式的字符，但它只能删除第一个匹配项。上面的例子可以这样写：
```
set str Hello World
string remove -r $str '[a-z]'
```
输出将是同样的`H W`。

## 深入了解
从历史的角度来看，匹配模式删除字符的概念来源于正则表达式，这是一个强大的工具，用于处理字符串。除了Fish Shell的`string`命令，我们也可以使用其他编程语言中的特殊函数来实现相同的功能，比如Python的`re`模块。

## 参考资料
- Fish Shell文档：https://fishshell.com/docs/current/cmds/string.html
- 正则表达式介绍：https://zh.wikipedia.org/wiki/正则表达式