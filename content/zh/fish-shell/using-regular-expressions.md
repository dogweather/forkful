---
title:    "Fish Shell: 使用正则表达式"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么

在使用鱼壳（Fish Shell）编程时，正则表达式是一种非常有用的工具。它可以帮助我们在文本中快速搜索、匹配和替换特定的模式。通过学习如何使用正则表达式，您可以提高编程效率，并且在处理文本数据时更加灵活。

## 如何使用

要在鱼壳中使用正则表达式，您首先需要在终端中输入命令 `fish_config` 来打开 Fish Shell 的配置页面。然后，选择 “Key Bindings” 选项，找到对应终端的快捷键，并将其修改为 `<Ctrl-R>`。这个快捷键将在 Fish Shell 中启用“历史命令”的功能。

现在，您可以按下 `<Ctrl-R>` 来搜索最近使用过的命令。但是，如果您想要更进一步，以模糊的字符来搜索命令，可以使用正则表达式进行匹配。以下是一个简单的例子：

```Fish Shell
2-5
grep -i "fish" ~/Documents/data.csv
cat ~/.bash_profile
```

输入 `<Ctrl-R>` 后，在输入栏中输入 `\d` 并按下回车键。这将匹配包含数字“2”、“3”、“4”或“5”的命令。您也可以使用其他字符来进行匹配，如 `.*` 表示任意字符。

## 深入探讨

正则表达式有很多不同的字符和用法，以适应不同的匹配需求。例如，用括号 `()` 来表示一个组，用 `|` 来实现或逻辑，用 `[]` 来表示字符的范围，用 `{n,m}` 来表示字符的重复次数等等。如果您想要进一步了解如何使用这些字符来构建复杂的模式匹配，请参考正则表达式的官方文档或在线教程。

## 看看这些

- Fish Shell 官方文档：https://fishshell.com/docs/current/
- 学习正则表达式：https://regexone.com/
- 使用 Fish Shell 中的文本处理命令：https://fishshell.com/docs/current/index.html#fish-string