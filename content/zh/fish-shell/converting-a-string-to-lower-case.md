---
title:    "Fish Shell: 将字符串转换为小写"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么
在编写代码时，有时我们需要将字符串转换为小写，这样可以方便我们进行大小写不敏感的匹配和操作。

## 如何实现
```Fish Shell
set my_string "HELLO"
echo $my_string | string tolower
```
输出：
```
hello
```

## 深入了解
Fish Shell提供了一个内置的`string tolower`命令来实现字符串的转换。该命令会将字符串中的所有字母转换为小写，并返回转换后的结果。如果要转换的字符串是变量，则需要使用`$`符号来引用该变量。

除了`string tolower`命令外，还可以使用Fish Shell的内置功能来实现字符串的转换。例如，可以使用`string match`命令来进行大小写不敏感的比较，以及使用`string replace`命令来替换字符串中的特定文本。

## 参考链接
[Fish Shell官方文档](https://fishshell.com/docs/current/cmds/string.html#tolower)
[《Fish Shell教程》](https://fishshell.com/docs/current/tutorial.html)
[Fish Shell的Github仓库](https://github.com/fish-shell/fish-shell)

## 参见
[字符串操作入门指南](https://www.jianshu.com/p/11a724a0ea1d)
[Fish Shell的中文文档](https://fishshell-cn.readthedocs.io/zh_CN/latest/)