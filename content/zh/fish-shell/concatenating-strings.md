---
title:                "Fish Shell: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

字符串拼接是编程中常见的操作，它可以让我们将多个文本或变量连接起来，构成一个更大的字符串。在Fish Shell中，我们可以使用内置的字符串操作函数和文本编辑器来实现字符串拼接，让我们的代码更加灵活和高效。

## 如何做

Fish Shell提供了多种方法来拼接字符串，让我们来看几个例子：

```Fish Shell
set greeting "你好"
set name "李华"
echo $greeting","$name"！"
```

在这个例子中，我们通过`$`符号将变量插入到字符串中，最终输出的结果是“你好，李华！”。

```Fish Shell
set sentence "我今天吃了"
set num 3
echo $sentence$num"个苹果。"
```

这个例子中，我们使用字符串拼接连接两个变量，输出的结果是“我今天吃了3个苹果。”。

如果想要拼接多个字符串，可以使用`string join`函数来实现：

```Fish Shell
set words "我"
string join " " $words "喜欢" "编程。"
```

最终输出的结果是“我喜欢编程。”，其中使用空格来连接字符串。

## 深入了解

Fish Shell中的字符串拼接还有其他更多的用法，可以通过使用转义字符、使用数组以及使用正则表达式来实现复杂的字符串操作。此外，我们还可以通过变量替换和命令替换来构建更加动态的字符串。

## 参考资料

- [Fish Shell官方文档-Strings](https://fishshell.com/docs/current/cmds/set.html#strings)
- [5个实用的字符串操作技巧](https://www.jianshu.com/p/2d908da25b13)
- [Fish Shell常用技巧之字符串操作](https://daily.fish/vscode-FishShell)