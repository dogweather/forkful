---
title:                "获取字符串的长度"
aliases:
- /zh/fish-shell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:13.710459-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?  
什么是字符串长度以及我们为什么需要它？字符串长度是字符的数量。程序员计算字符串长度来处理文本数据，例如验证输入或者拼接字符串。

## How to:
在Fish Shell中，获取字符串长度非常直接。你可以使用 `string length` 命令。看下面的例子：

```Fish Shell
# 举个例子，我们要找到 "你好，世界" 的长度
set str "你好，世界"
string length $str
```

输出将会是：

```
6
```

注意：中文字符通常被认为是一个字符。

## Deep Dive
在Fish Shell的早期版本中，找出字符串的长度可能需要依赖于外部工具，像`wc`命令。Fish Shell `string`这个内建命令提供了一个简单且原生的方法来处理字符串。

比较另一个常用的Shell，如Bash，它需要通过`${#variable}`语法来获取字符串长度。Fish Shell的设计哲学更倾向于直观的命令。

从实现细节来看，Fish Shell处理字符串长度时正确处理了多字节字符。这意味着即使是中文、日文或其他Unicode字符，也都能精确计算长度。

## See Also
- 官方文档中关于`string`命令的更多信息：[Fish Shell string documentation](https://fishshell.com/docs/current/cmds/string.html)
- 其他Shell字符串操作的比较：[Comparing string operations in different shells](https://hyperpolyglot.org/unix-shells#strings)
