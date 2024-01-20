---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？
删除匹配模式的字符是指在字符串中找到并清除所有与给定模式匹配的字符。程序员执行此操作以进行数据清理或修改字符串。

## 如何操作:
在Fish Shell中，我们可以使用`string match`命令来删除匹配模式的字符。看下面的例子:
```Fish Shell
set sentence "我爱吃苹果和香蕉。"
set pattern "*苹果*"
string match -vr $pattern $sentence
```
在此例子中，原始句子是 "我爱吃苹果和香蕉。"。模式是 "*苹果*"，意为任何含"苹果"的子串。 `-v` 选项意为 invert，即只保留不匹配的部分。 `-r` 选项表示 regex，用来做正则模式匹配。这段代码会输出 "我爱吃和香蕉。"。

## 深入了解:
Fish Shell是在21世纪初开始开发的，出于对历史Shell设计理念的挑战。删除匹配模式的字符功能是用于方便处理字符级操作的基础命令。

还有许多其他方法可以删除匹配模式的字符，如使用`sed`命令或使用高级语言如Python，但`string match`是Fish Shell原生支持的方法。

Fish Shell 使用正则表达式来匹配字符串。这是一种模式匹配系统，能处理复杂的匹配需求。

## 参考资料:
删除匹配模式的字符是一个广泛的话题，如果你想要深入了解，请参阅以下资源：

1. [Fish Shell文档](https://fishshell.com/docs/current/index.html)
2. [正则表达式教程](https://www.lynda.com/Regular-Expressions-tutorials/Using-Regular-Expressions/85870-2.html)
3. [Python文档](https://docs.python.org/3/library/re.html)