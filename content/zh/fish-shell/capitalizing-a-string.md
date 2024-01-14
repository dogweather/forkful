---
title:                "Fish Shell: 将字符串大写"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：在某些情况下，对字符串进行大写处理是必需的，比如在编写注释或打印日志时。 大写字符串可以使文本更容易阅读和理解。

Fish Shell 如何：要在 Fish Shell 中对字符串进行大写处理，使用内置的 `string capitalize` 命令。它可以接受一个或多个字符串参数，并将它们大写。例如：
```Fish Shell
set my_string "hello"
echo (string capitalize $my_string)
```
执行上述代码，输出将会是 `Hello`。

## 深入了解
除了内置的 `string capitalize` 命令，还可以通过使用内置的 `string sub` 命令来实现对字符串的大写处理。这个命令可以接受一个正则表达式作为参数，并将匹配到的字符串替换为另一个字符串。因此，我们可以使用 `string sub` 命令将字符串的第一个字符替换为大写。

例如，假设我们有一个叫 `my_string` 的变量，它的值是一个小写的字符串。我们可以使用下面的命令将其第一个字符大写，并赋值给一个新的变量 `new_string`：
```Fish Shell
set my_string "hello"
set new_string (string sub "^(.*)" (string to-upper \1) $my_string)
echo $new_string
```
这样的话，输出将会是 `Hello`。

## 另请参阅
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [Fish Shell 教程](https://fishshell.com/docs/current/tutorial.html)